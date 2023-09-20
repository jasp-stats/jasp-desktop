import sys
from pathlib import Path
import re
import argparse

htmlPrefix = """JASP depends on the following R packages:
<table class="table table-bordered table-hover table-condensed">
<thead>
<tr>
<th title="Field #1">Package</th>
<th title="Field #2">Version</th>
<th title="Field #3">Author</th>
</tr>
</thead>
<tbody>
"""

htmlPostfix = """</tbody>
</table>
&nbsp;"""

def extract_data(descriptionPath):
    description = descriptionPath.read_text(encoding = 'unicode-escape')
    encodingMatch = re.search('(^Encoding:)\s*(.*$)', description, flags=re.MULTILINE)
    encoding = 'utf-8'
    if encodingMatch:
        encoding = encodingMatch.group(2)

    description = descriptionPath.read_text(encoding = encoding)
    package = re.search('(^Package:)\s*(.*$)', description, flags=re.MULTILINE).group(2)
    version = re.search('(^Version:)\s*(.*$)', description, flags=re.MULTILINE).group(2)
    
    authorMatch = re.search('(^Author:)\s*(.*$)', description, flags=re.MULTILINE)
    author = authorMatch.group(2)
    for line in description[authorMatch.end() + 1: -1].splitlines():
        match = re.search('^\s+(.*$)', line, flags=re.MULTILINE)
        if match == None:
            break
        author += '\n' + match.group(1)
    return [package, version, author]

def gather_pkgs(path_str):
    path = Path(path_str)
    renv_cache = path / 'Modules' / 'renv-cache' / 'v5'
    if(not renv_cache.exists()):
        print('renv-cache not found')
        return

    #Gather and sort alphabetically on pkg name
    pkgList = []
    descriptions = set(renv_cache.glob("*/*/*/*/DESCRIPTION"))
    for desc in descriptions:
        pkgList.append(list(extract_data(desc)))
    pkgList.sort(key=lambda x: x[0])

    #write html table
    with open(path.parts[-1] + '-pkgs.txt', 'w') as out:
        out.write(htmlPrefix)
        for pkg in pkgList:
            out.write('<tr>\n')
            out.write('<td> ' + pkg[0] + ' </td>\n')
            out.write('<td> ' + pkg[1] + ' </td>\n')
            out.write('<td> ' + pkg[2] + ' </td>\n')
            out.write('</tr>\n')
        out.write(htmlPostfix)

def main():
    parser = argparse.ArgumentParser(
                    prog='python3 pkglist',
                    description='Takes a Jasp build dir and outputs all R packages used as HTML table',
                    epilog='example: python3 pkglist.py build-jasp-Desktop-Qt...')
    parser.add_argument('jasp_build_dir')
    args = parser.parse_args()
    gather_pkgs(args.jasp_build_dir)

if __name__ == "__main__":
    main()
