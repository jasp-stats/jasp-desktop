#!/usr/bin/python3

from pathlib import Path
import argparse
import json
import sys
from github import Github
from github import Auth
import requests
import os
from time import sleep

def gatherMod(repo_list, token, include_prerelease=False, flatpak=False, download_on=False):
    auth = Auth.Token(token)

    g = Github(auth=auth)

    windows = []
    mac_intel = []
    mac_arm = []
    flatpak_intel = []

    for repo_str in repo_list:
        try:
            repo = g.get_repo(repo_str)
            if not include_prerelease:
                release = repo.get_latest_release()
            else:
                releases = repo.get_releases()
                release = releases[0]

            for asset in release.assets:
                asset.complete()
                if "windows" in asset.name.lower():
                    windows.append(asset)          
                if "macos" in asset.name.lower() and "x86" in asset.name.lower():
                    mac_intel.append(asset)
                if "macos" in asset.name.lower() and "arm64" in asset.name.lower():
                    mac_arm.append(asset)
                if "flatpak" in asset.name.lower() and "x86" in asset.name.lower():
                    flatpak_intel.append(asset)
        except:
            print("Could not parse module: " + repo_str, file=sys.stderr)       

    g.close()

    if not flatpak:
        result = {
            "Windows-x86_64": [{"url": x.browser_download_url, "checksum": x.digest[7:]} for x in windows],
            "MacOS-arm64": [{"url": x.browser_download_url, "checksum": x.digest[7:]} for x in mac_arm],
            "MacOS-x86_64": [{"url": x.browser_download_url, "checksum": x.digest[7:]} for x in mac_intel]
        }
        print(json.dumps(result, indent=4))


    def download(download_url, token = ""):
        headers = {
        "Authorization": f"Bearer {token}",
        "Accept": "application/vnd.github.v3+json"
        }
        with requests.get(download_url, stream=True, headers=headers) as response:
            response.raise_for_status()
            with open(os.path.basename(download_url), "wb") as f:
                for chunk in response.iter_content(chunk_size=8192): 
                    f.write(chunk)

    if flatpak:
        print(flatpak_intel)
        for x in flatpak_intel:
            download(x.browser_download_url, token)
    if not flatpak and download_on:
        for x in mac_intel + mac_arm + windows:
            download(x.browser_download_url, token)

        



def main():
    parser = argparse.ArgumentParser(prog='gatherModuleJson', description='Generates module json from a dir containing jasp submodules')
    parser.add_argument('dir')
    parser.add_argument('token')
    parser.add_argument('--prerelease', action='store_true')
    parser.add_argument('--flatpak', action='store_true')
    parser.add_argument('--download', action='store_true')
    args = parser.parse_args()
    path = Path(args.dir) 
    token = args.token
    modules = ['jasp-stats-modules/' + y.name for y in path.glob('core-modules/*') if y.is_dir()] + ['jasp-stats-modules/' + y.name for y in path.glob('jasp-modules/*') if y.is_dir()]
    if args.prerelease:
        modules +=  ['jasp-stats-modules/' + y.name for y in path.glob('beta-modules/*') if y.is_dir()]
    print(F"Gathering modules: {modules}")
    gatherMod(modules, token, args.prerelease, args.flatpak, args.download)


if __name__ == "__main__":
    main()
