#!/usr/bin/env python3

from pathlib import Path
import argparse
import tarfile
import concurrent.futures
from datetime import datetime
import platform
import re
from shutil import copytree
import os
import tempfile
import zipfile

def generatePkgArchiveName(pkg, inner_extension):
    descriptionPath = Path(pkg) / 'DESCRIPTION'
    with descriptionPath.open() as f:
        description = f.read()
        name = re.search('(^Package:)\\s*(.*$)', description, flags=re.MULTILINE).group(2)
        type = re.search('(^RemoteType:)\\s*(.*$)', description, flags=re.MULTILINE)
        if type and type.group(2) == 'github':
            version = re.search('(^RemoteSha:)\\s*(.*$)', description, flags=re.MULTILINE).group(2)
        else:
            version = re.search('(^Version:)\\s*(.*$)', description, flags=re.MULTILINE).group(2)
        return name + '_' + version + inner_extension

def cellar_name():
    time = datetime.now().strftime("%d-%m-%Y_%H_%M")
    os = platform.system()
    if os == 'Darwin':  os = 'macOS'
    return 'cellar_' + os + '_' + platform.machine() + '.tar.gz'

def create_zip(outPath, inPath, root):
    with zipfile.ZipFile(outPath, 'w') as zip:
        for dir, dirs, files in inPath.walk(top_down=False):
            for file in files:
                zip.write(dir / file, root / (dir / file).relative_to(inPath))


def create_archive(pkg):
    with tempfile.TemporaryDirectory() as tmpdir:
        #we need to remove the "Built: " line from DESCRIPTION otherwise it breaks cp on windows because R is really weird
        tmpdir = Path(tmpdir)
        copytree(pkg[0], tmpdir, dirs_exist_ok=True)
        descriptionPath = tmpdir / 'DESCRIPTION'
        with descriptionPath.open('r+') as description:
            lines = [x for x in description.readlines() if not x.startswith('Built: ')]
            description.seek(0)
            description.truncate()
            description.writelines(lines)
        # MD5Path = tmpdir / 'MD5'
        # MD5Path.unlink(missing_ok=True)
        if 'zip' not in pkg[1].suffix:
            with tarfile.open(pkg[1], 'w:gz') as tar:
                tar.add(tmpdir, arcname=pkg[0].name)
        else:
            create_zip(pkg[1], tmpdir, pkg[0].name)

def build_cellar(renv_cache, inner_extension):
    renv_cache = Path(renv_cache) / 'v5'
    if(not renv_cache.exists()):
        print('renv-cache not found')
        return
    
    output_dir = Path('.') / 'cellar'
    output_dir.mkdir(exist_ok=True)
    
    pkgs = list(renv_cache.glob('*/*/*/*'))
    pkgsOutPaths = [output_dir / generatePkgArchiveName(x, inner_extension) for x in pkgs]        
    with concurrent.futures.ProcessPoolExecutor(max_workers=4) as executor:
        executor.map(create_archive, zip(pkgs, pkgsOutPaths), chunksize=50)
    # for x in zip(pkgs, pkgsOutPaths):
    #     create_archive(x)

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
    parser.add_argument('--inner_extension', required=False)
    args = parser.parse_args()
    extension = args.inner_extension if  args.inner_extension != None else '.tar.gz'
    build_cellar(args.renv_cache, extension)

if __name__ == "__main__":
    main()
