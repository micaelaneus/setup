import os
import subprocess

def remotepasseval(path):
    HOME = os.environ['HOME']
    path = path.format(HOME=HOME)
    return subprocess.check_output(['gpg', '--quiet', '--batch', '--decrypt', path]).strip()
