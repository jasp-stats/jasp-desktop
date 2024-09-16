#!/usr/bin/env python3

from pathlib import Path
import argparse
import tarfile
import concurrent.futures
from datetime import datetime
import platform
import re
from shutil import copytree
import tempfile

def generatePkgArchiveName(pkg):
    descriptionPath = Path(pkg) / 'DESCRIPTION'
    with descriptionPath.open() as f:
        description = f.read()
        name = re.search('(^Package:)\\s*(.*$)', description, flags=re.MULTILINE).group(2)
        type = re.search('(^RemoteType:)\\s*(.*$)', description, flags=re.MULTILINE)
        if type and type.group(2) == 'github':
            version = re.search('(^RemoteSha:)\\s*(.*$)', description, flags=re.MULTILINE).group(2)
        else:
            version = re.search('(^Version:)\\s*(.*$)', description, flags=re.MULTILINE).group(2)
        return name + '_' + version + '.tar.gz'

def cellar_name():
    time = datetime.now().strftime("%d-%m-%Y_%H_%M")
    os = platform.system()
    if os == 'Darwin':  os = 'macOS'
    return 'cellar_' + os + '_' + platform.machine() + '.tar.gz'

def make_tar(pkg):
    with tarfile.open(pkg[1], 'w:gz') as tar, tempfile.TemporaryDirectory() as tmpdir:
        #we need to remove the "Built: " line from DESCRIPTION otherwise it breaks cp on windows because R is really weird
        copytree(pkg[0], tmpdir, dirs_exist_ok=True)
        descriptionPath = Path(tmpdir) / 'DESCRIPTION'
        with descriptionPath.open('r+') as description:
            lines = [x for x in description.readlines() if not x.startswith('Built: ')]
            description.seek(0)
            description.truncate()
            description.writelines(lines)
        tar.add(tmpdir, arcname=pkg[1].name)

def build_cellar(renv_cache):
    renv_cache = Path(renv_cache) / 'v5'
    if(not renv_cache.exists()):
        print('renv-cache not found')
        return
    
    output_dir = Path('.') / 'cellar'
    output_dir.mkdir(exist_ok=True)
    
    pkgs = list(renv_cache.glob('*/*/*/*'))
    pkgsOutPaths = [output_dir / generatePkgArchiveName(x) for x in pkgs]        
    with concurrent.futures.ProcessPoolExecutor(max_workers=4) as executor:
        executor.map(make_tar, zip(pkgs, pkgsOutPaths), chunksize=50)
    # for x in zip(pkgs, pkgsOutPaths):
    #     make_tar(x)

    output_tar = Path('.') / 'cellar.tar.gz'
    output_tar.unlink(missing_ok=True)
    with tarfile.open(Path('.') / cellar_name(), 'w:gz') as tar:
        tar.add(output_dir)


def main():
    parser = argparse.ArgumentParser(
                    prog='python3 createCellarFromRenvCache',
                    description='Takes a renv-cache dir and outputs an renv-cellar',
                    epilog='example: python3 createCellarFromRenvCache.py /System/Applications/JASP.app/Contents/Modules/renv-cache/')
    parser.add_argument('renv_cache')
    args = parser.parse_args()
    build_cellar(args.renv_cache)

if __name__ == "__main__":
    main()
