import collections
from pathlib import Path
from subprocess import Popen, PIPE
import platform, os , io, uuid, psutil, time
from .utilities import *
from .bash import rmrf 
import copy 

class CPTError(Exception):
    def __init__(self, message):            
        super().__init__(message)
            

default_output_files = {
    #original files
    'original_predictor': 'original_predictor',
    'out_of_sample_predictor': 'original_forecast_predictor',
    'original_predictand': 'original_predictand',

    # goodness 
    'goodness_index': 'goodness_index',

    #pattern
    'cca_x_timeseries': 'predictor_cca_timeseries',
    'cca_y_timeseries': 'predictand_cca_timeseries',
    'cca_canonical_correlation':  'cca_canonical_correlation',
    'eof_x_timeseries': 'predictor_eof_timeseries',
    'eof_x_explained_variance': 'predictor_explained_variance',
    'eof_y_explained_variance': 'predictand_explained_variance',
    'eof_y_timeseries':  'predictand_eof_timeseries',
    'eof_x_loadings': 'predictor_eof_spatial_loadings',
    'eof_y_loadings': 'predictand_eof_spatial_loadings',
    'cca_x_loadings': 'predictor_cca_spatial_loadings',
    'cca_y_loadings': 'predictand_cca_spatial_loadings',

    # hindcasts
    'hindcast_values': 'hindcast_values',
    'hindcast_probabilities': 'hindcast_probabilities',
    'hindcast_prediction_error_variance': 'hindcast_prediction_error_variance',
    #forecasts 
    'forecast_probabilities': 'forecast_probabilities',
    'forecast_values': 'forecast_values',
    'forecast_prediction_error_variance': 'forecast_prediction_error_variance',

    # skill
    'pearson': 'pearson', 
    'spearman': 'spearman', 
    'two_alternative_forced_choice': 'two_alternative_forced_choice',
    'pct_variance': 'pct_variance', 
    'variance_ratio': 'variance_ratio', 
    'mean_bias': 'mean_bias', 
    'root_mean_squared_error': 'root_mean_squared_error',
    'mean_absolute_error': 'mean_absolute_error',
    'hit_score': 'hit_score',
    'hit_skill_score': 'hit_skill_score',
    'leps': 'leps', 
    'gerrity_score': 'gerrity_score', 
    '2afc_fcst_categories': '2afc_fcst_categories', 
    '2afc_continuous_fcsts': '2afc_continuous_fcsts',
    'roc_area_below_normal': 'roc_area_below_normal',
    'roc_area_above_normal': 'roc_area_above_normal',
    'generalized_roc': 'generalized_roc', 
    'rank_probability_skill_score': 'rank_probability_skill_score', 
    'ignorance': 'ignorance', 
}


class CPT:
    def __init__(self, interactive=False,  log=None, project_file=None,  record=None, output_files=default_output_files, outputdir=None, **kwargs):
        # TODO why is **kwargs here? It's not used, but removing it
        # could be a breaking change for anyone who's currently
        # passing in kwargs that are being ignored.
        self.interactive = interactive
        self.last_message = 'has not started yet'

        if platform.system() == 'Windows':
            exe_name = 'CPT_batch.exe'
        else:
            exe_name = 'CPT.x'
        self.last_cmd = exe_name
        self.cpt = str(Path(os.getenv('CPT_BIN_DIR')) / exe_name)
        assert Path(self.cpt).is_file(), 'CPT executable not found'
        
        if outputdir is None:
            self.set_outputdir = False
            if not (Path.home() / '.pycpt_workspace').is_dir():
                (Path.home() / '.pycpt_workspace').mkdir(exist_ok=True, parents=True)
            
            self.id = str(uuid.uuid4())
            self.outputdir = Path.home() / '.pycpt_workspace' /  self.id
            self.outputdir.mkdir(exist_ok=True, parents=True)
            self.outputs = copy.deepcopy(output_files)
            for key in self.outputs.keys():
                self.outputs[key] = self.outputdir / self.outputs[key]
        else:
            self.set_outputdir = True
            self.outputdir = Path(outputdir)
            self.outputdir.mkdir(exist_ok=True, parents=True)
            self.outputs = copy.deepcopy(output_files)
            for key in self.outputs.keys():
                self.outputs[key] = self.outputdir / self.outputs[key]

        self.record = str(Path(record).absolute()) if record else None 
        self.log = str(Path(log)) if log else None 
        self.project_file = str(Path(project_file)) if project_file else None 
        self.cpt_process = Popen([Path(self.cpt).absolute()], stdin=PIPE, stderr=PIPE, stdout=PIPE)
        self.reader = io.TextIOWrapper(self.cpt_process.stdout, encoding='utf8')
        x = self.read()
        self.last_message = x
        if self.interactive:
            print(x)
        self.write(572) # don't repeat options listing after every prompt
        self.write(574) # no progress meter
        self.write(571) # error handling
        self.write(3)   #   stop
        
    def read(self):
        output = io.StringIO()
        buf = collections.deque(maxlen=len(CPT_SECRET))
        while True:
            char = self.reader.read(1)
            if char == "":
                break
            output.write(char)
            buf.append(char)
            if ''.join(buf) == CPT_SECRET or self.cpt_process.poll() is not None:
                break
        return output.getvalue()


    def status(self ):
        return self.cpt_process.poll() is None

    def write(self, cpt_cmd):
        if self.interactive: 
            print(str(cpt_cmd))
        cpt_cmd = str(cpt_cmd)

        if cpt_cmd[-1] != os.linesep:
            cpt_cmd += os.linesep

        try:
            if self.project_file:
                with open(self.project_file, 'a') as f: 
                    f.write(cpt_cmd)
            self.cpt_process.stdin.write(cpt_cmd.encode())
            self.cpt_process.stdin.flush() 
        except Exception as e: 
            cptalive = self.cpt_process.poll() is None
            msg = "\nPROCESS STATUS: {}\n".format("ALIVE" if cptalive else "DEAD")
            msg += "  last command: '{}'\n".format(self.last_cmd.strip())
            msg += "  last message:'{}'\n".format(self.last_message.strip())

            raise CPTError(msg)
            
            
        if self.log is not None:
            log = open(self.log, 'a')
            log.write(cpt_cmd + '\n')
            log.close()
        x = self.read()
        if 'Output Results\n  \n' in x:
            while "0. Exit" not in x:
                x += self.read()
        if 'ERROR:' in x: 
            cptalive = self.cpt_process.poll() is None
            msg = "\nPROCESS STATUS: {}\n".format("ALIVE (WILL BE STOPPED)" if cptalive else "DEAD")
            msg += "  last command: '{}'\n".format(cpt_cmd.strip())
            msg += "  last message:'{}'\n".format(x.strip())
            self.kill()
            raise CPTError(msg)
        if self.interactive:
            print(x)
        if self.log is not None:
            log = open(self.log, 'a')
            log.write(x + '\n')
            log.close()
        self.last_message = x
        self.last_cmd = cpt_cmd
        return self.status()

    def execute_script(self, script):
        assert Path(script).is_file()
        with open(script, 'r') as f: 
            script = f.read().split(os.linesep)
            for command in script:
                self.write(command)

    def kill(self):
        self.cpt_process.kill()
   
    def clean(self):
        if self.outputdir.is_dir() and not self.set_outputdir:
            rmrf(self.outputdir)
        self.kill()

    def wait_for_files(self, isdel=False):
        try:
            proc = psutil.Process(pid=self.cpt_process.pid)
            while len(proc.open_files()) > 0: 
                time.sleep(0.1)
        except Exception as e:
            if isinstance(e, psutil.NoSuchProcess):
                if not isdel:
                    print( "CPT ProcessID #{} does not exist - seems like CPT died early. cleaning data from {}.".format(self.cpt_process.pid, self.outputdir))
                self.clean()

    def __del__(self):
        self.wait_for_files(isdel=True)
        self.clean()
