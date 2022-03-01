from pathlib import Path
from subprocess import Popen, PIPE
import platform, os , io, uuid
from .utilities import * 

class CPT:
    def __init__(self, cpt_directory=None, version=CPT_DEFAULT_VERSION, interactive=False,  log=None, project_file=None,  record=None, **kwargs):
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
            if self.log is not None:
                log = open(self.log, 'a')
                log.write(std_out + '\n')
                log.close()
            return std_out
        except:
            self.kill()
            return std_out

    def status(self ):
        print('CPT is {}'.format('alive' if self.cpt_process.poll() is None else 'dead'))
        return self.cpt_process.poll() is None

    def write(self, cpt_cmd):
        if self.interactive: 
            print(str(cpt_cmd))
        cpt_cmd = str(cpt_cmd)

        if cpt_cmd[-1] != os.linesep:
            cpt_cmd += os.linesep

        self.last_cmd = cpt_cmd
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
        x = ''
        if cpt_cmd == '111' + os.linesep:
            while "0. Exit" not in x:
                x += self.read()
        else:
            x += self.read()
        if self.interactive:
            print(x)
        return x

    def execute_script(self, script):
        assert Path(script).is_file()
        with open(script, 'r') as f: 
            script = f.read().split(os.linesep)
            for command in script:
                self.write(command)

    def kill(self):
        self.cpt_process.kill()
   
    
        
