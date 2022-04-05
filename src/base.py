from pathlib import Path
from subprocess import Popen, PIPE
import platform, os , io, uuid, psutil, time
from .utilities import *
from .bash import rmrf 
import copy 

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
    '2afc': '2afc',
    'pct_variance': 'pct_variance', 
    'variance_ratio': 'variance_ratio', 
    'mean_bias': 'mean_bias', 
    'root_mean_squared_error': 'mean_bias', 
    'mean_absolute_error': 'mean_bias', 
    'hit_score': 'mean_bias', 
    'hit_skill_score': 'mean_bias', 
    'leps': 'leps', 
    'gerrity_score': 'gerrity_score', 
    '2afc_fcst_categories': '2afc_fcst_categories', 
    '2afc_continuous_fcsts': '2afc_continuous_fcsts',
    'roc_below': 'roc_below', 
    'roc_above': 'roc_above', 
    'generalized_roc': 'generalized_roc', 
    'rank_probability_skill_score': 'rank_probability_skill_score', 
    'ignorance': 'ignorance', 
}


class CPT:
    def __init__(self, cpt_directory=None, version=CPT_DEFAULT_VERSION, interactive=False,  log=None, project_file=None,  record=None, output_files=default_output_files, **kwargs):
        if cpt_directory is None: 
            cpt_directory = find_cpt(version=version)
            if not cpt_directory:
                cpt_directory = install_cpt_windows(version=version) if platform.system() == 'Windows' else install_cpt_unix(version=version) 
        self.cptdir = Path(cpt_directory)
        assert self.cptdir.is_dir(), 'CPT directory does not exist'
        self.last_cmd = 'CPT.x'
        self.interactive = interactive

        if platform.system() == 'Windows':
            self.cpt = str(Path(self.cptdir / 'CPT_batch.exe').absolute())
        else:
            self.cpt = str(Path(self.cptdir / 'CPT.x').absolute())
        assert Path(self.cpt).is_file(), 'CPT executable not found'
        if not (Path.home() / '.pycpt_workspace').is_dir():
            (Path.home() / '.pycpt_workspace').mkdir(exist_ok=True, parents=True)
        
        self.id = str(uuid.uuid4())
        self.outputdir = Path.home() / '.pycpt_workspace' /  self.id
        self.outputdir.mkdir(exist_ok=True, parents=True)
        self.outputs = copy.deepcopy(output_files)
        for key in self.outputs.keys():
            self.outputs[key] = self.outputdir / self.outputs[key]

        os.environ['CPT_BIN_DIR'] = str(self.cptdir.absolute())   
        self.record = str(Path(record).absolute()) if record else None 
        self.log = str(Path(log)) if log else None 
        self.project_file = str(Path(project_file)) if project_file else None 
        self.cpt_process = Popen([Path(self.cpt).absolute()], stdin=PIPE, stderr=PIPE, stdout=PIPE)
        self.reader = io.TextIOWrapper(self.cpt_process.stdout, encoding='utf8')
        x = self.read()
        if self.interactive:
            print(x)
        self.write(571)
        self.write(3)
        
    def read(self):
        try: 
            std_out = ""
            queue = list("           ")
            while CPT_SECRET not in "".join(queue[::-1]) and self.cpt_process.poll() is None:
                char = self.reader.read(1)
                queue.pop(-1)
                queue.insert(0, char)
                std_out += char
            assert self.cpt_process.poll() is None, 'whoops cpt died'
            return std_out
        except:
            self.kill()
            return std_out

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
            raise ValueError('CPT Write Error')
        if self.log is not None:
            log = open(self.log, 'a')
            log.write(cpt_cmd + '\n')
            log.close()
        x = self.read()
        if 'Output Results\n  \n' in x:
            while "0. Exit" not in x:
                x += self.read()
            
        if self.interactive:
            print(x)
        if self.log is not None:
            log = open(self.log, 'a')
            log.write(x + '\n')
            log.close()
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
        rmrf(self.outputdir)

    def wait_for_files(self):
        proc = psutil.Process(pid=self.cpt_process.pid)
        while len(proc.open_files()) > 0: 
            time.sleep(0.1)
    
    def __del__(self):
        self.wait_for_files()
        self.clean()