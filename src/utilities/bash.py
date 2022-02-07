### General Purpose Utilities 
def rmrf(dirn):
    subfiles = [file for file in dirn.glob('*') if file.is_file()]
    subdirs = [diro for diro in dirn.glob('*') if diro.is_dir()]

    for file in subfiles: 
        file.unlink() 
    for subdir in subdirs: 
        try: 
            subdir.rmdir() 
        except: 
            rmrf(subdir)
    dirn.rmdir() 

def rmstar(dirn):
    subfiles = [file for file in dirn.glob('*') if file.is_file()]
    for file in subfiles: 
        file.unlink() 

def ls_files_recursive(dirn):
    subfiles = [file for file in dirn.glob('*') if file.is_file()]
    subdirs = [diro for diro in dirn.glob('*') if diro.is_dir()]
    files = [ file.absolute() for file in subfiles]
    for subdir in subdirs: 
        files.extend(ls_files_recursive(subdir))
    return files
