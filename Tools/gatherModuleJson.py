#!/usr/bin/python3

from pathlib import Path
import argparse
import json
import sys
from github import Github
from github import Auth




def gatherMod(repo_list, token, include_prerelease=True):
    auth = Auth.Token(token)

    g = Github(auth=auth)

    windows = []
    mac_intel = []
    mac_arm = []

    for repo_str in repo_list:
        try:
            repo = g.get_repo(repo_str)
            if include_prerelease:
                release = repo.get_latest_release()
            else:
                releases = repo.get_releases()
                release = releases[0]
            
            for asset in release.assets:
                if "windows" in asset.name.lower():
                    windows.append(asset)
                elif "mac" in asset.name.lower() and "x86" in asset.name.lower():
                    mac_intel.append(asset)
                elif "mac" in asset.name.lower() and "arm64" in asset.name.lower():
                    mac_arm.append(asset)
        except:
            print("Could not parse module: " + repo_str, file=sys.stderr)       

    g.close()

    result = {
        "Windows-x86_64": [{"url": x.browser_download_url, "checksum": x.digest[7:]} for x in windows],
        "MacOS-arm64": [{"url": x.browser_download_url, "checksum": x.digest[7:]} for x in mac_arm],
        "MacOS-x86_64": [{"url": x.browser_download_url, "checksum": x.digest[7:]} for x in mac_intel]
    }

    print(json.dumps(result, indent=4))


def main():
    parser = argparse.ArgumentParser(prog='gatherModuleJson', description='Generates module json from a dir containing jasp submodules')
    parser.add_argument('dir')
    parser.add_argument('token')
    parser.add_argument('--release', action='store_true')
    args = parser.parse_args()
    path = Path(args.dir) 
    token = args.token
    modules = ['jasp-stats-modules/' + y.name for y in path.glob('*') if y.is_dir()]
    gatherMod(modules, token, args.release)


if __name__ == "__main__":
    main()
