import datetime
import ftplib
import ftputil
from pathlib import Path


def _affirmative(s):
    return s.lower().startswith('y')

def interactive_sync(local_root, host, remote_root, user, password):
    session_factory = ftputil.session.session_factory(base_class=ftplib.FTP_TLS)
    with ftputil.FTPHost(host, user, password, session_factory=session_factory) as ftp:
        print('Scanning files...')
        new_files = sorted(scan_dir(local_root, remote_root, Path('.'), ftp))
        if len(new_files) == 0:
            print('Everything up to date.')
            return

        print('New files:')
        for relpath in new_files:
            print('    ', relpath)

        answer = input("Upload all files? [y/n] ")
        if _affirmative(answer):
            for relpath in new_files:
                print('uploading', relpath)
                upload_file(local_root / relpath, remote_root / relpath, ftp)
            return

        answer = input("Upload some files? [y/n] ")
        if _affirmative(answer):
            for relpath in new_files:
                answer = input(f"Upload {relpath} ? [y/n] ")
                if _affirmative(answer):
                    upload_file(local_root / relpath, remote_root / relpath, ftp)


def scan_dir(local_root, remote_root, relpath, ftp):
    local_dir = local_root / relpath
    assert local_dir.is_dir()
    remote_dir = remote_root / relpath

    new_files = []
    if ftp.path.exists(remote_dir.as_posix()):
        assert ftp.path.isdir(remote_dir.as_posix())
    else:
        ftp.makedirs(remote_dir.as_posix())
    for local_item in local_dir.iterdir():
        item_relpath = local_item.relative_to(local_root)
        if local_item.is_dir():
            new_files += scan_dir(local_root, remote_root, item_relpath, ftp)
        else:
            assert local_item.is_file()
            if is_new_file(remote_root / item_relpath, ftp):
                new_files.append(item_relpath)
    return new_files

    
def is_new_file(remote_path, ftp):
    p = remote_path.as_posix()
    if ftp.path.exists(p):
        assert ftp.path.isfile(p)
        return False
    else:
        return True


def upload_file(local, remote, ftp):
    ftp.upload(local, remote.as_posix())
