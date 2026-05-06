#!/usr/bin/python3

from pathlib import Path
import argparse
import json
import sys
from github import Github, Auth
import requests
import os
from datetime import datetime


def classify_asset(asset_name):
    name = asset_name.lower()
    
    if "windows" in name:
        return "windows"
    
    if "flatpak" in name and "x86" in name:
        return "flatpak"
        
    if "macos" in name:
        if "arm64" in name:
            return "mac_arm"
        if "x86" in name:
            return "mac_intel"
            
    return None

def find_latest_platform_assets(repo, include_prerelease=False, scan_depth=10):
    best = {
        "windows":   (None, datetime.min),
        "mac_intel": (None, datetime.min),
        "mac_arm":   (None, datetime.min),
        "flatpak":   (None, datetime.min),
    }

    releases = repo.get_releases()
    checked_count = 0

    for release in releases:
        if checked_count >= scan_depth:
            break
        
        if not include_prerelease and release.prerelease:
            continue

        checked_count += 1
        
        for asset in release.get_assets():
            platform = classify_asset(asset.name)
            
            if platform:
                asset_date = asset.updated_at.replace(tzinfo=None)
                current_best_date = best[platform][1]
                
                if asset_date > current_best_date:
                    best[platform] = (asset, asset_date)

    return {k: v[0] for k, v in best.items()}



def gatherMod(repo_list, token, include_prerelease=False, flatpak=False, download_on=False):
    auth = Auth.Token(token)
    g = Github(auth=auth)

    results = {
        "windows": [],
        "mac_intel": [],
        "mac_arm": [],
        "flatpak": []
    }

    for repo_str in repo_list:
        try:
            repo = g.get_repo(repo_str)
            best_assets = find_latest_platform_assets(repo, include_prerelease)
            
            missing_platforms = [platform for platform, asset in best_assets.items() if asset is None]
            
            if missing_platforms:
                raise ValueError(f"Missing assets for platforms: {', '.join(missing_platforms)}")

            results["windows"].append(best_assets["windows"])
            results["mac_intel"].append(best_assets["mac_intel"])
            results["mac_arm"].append(best_assets["mac_arm"])
            results["flatpak"].append(best_assets["flatpak"])
            
        except Exception as e:
            print(f"Could not parse module {repo_str}: {e}", file=sys.stderr)
            sys.exit(1)

    g.close()

    def download(download_url, token=""):
        headers = {
            "Authorization": f"Bearer {token}",
            "Accept": "application/octet-stream"
        }
        print(f"Downloading {os.path.basename(download_url)}...", file=sys.stderr)
        with requests.get(download_url, stream=True, headers=headers) as response:
            response.raise_for_status()
            with open(os.path.basename(download_url), "wb") as f:
                for chunk in response.iter_content(chunk_size=8192): 
                    f.write(chunk)

    if not flatpak:
        win_list = [{"url": x.browser_download_url, "checksum": getattr(x, 'digest', '')[7:]} for x in results["windows"]]
        mac_arm_list = [{"url": x.browser_download_url, "checksum": getattr(x, 'digest', '')[7:]} for x in results["mac_arm"]]
        mac_intel_list = [{"url": x.browser_download_url, "checksum": getattr(x, 'digest', '')[7:]} for x in results["mac_intel"]]

        json_output = {
            "Windows-x86_64": sorted(win_list, key=lambda item: item["url"]),
            "MacOS-arm64":    sorted(mac_arm_list, key=lambda item: item["url"]),
            "MacOS-x86_64":   sorted(mac_intel_list, key=lambda item: item["url"])
        }
        print(json.dumps(json_output, indent=4))

    if flatpak:
        for x in results["flatpak"]:
            download(x.browser_download_url, token)
            
    if not flatpak and download_on:
        all_assets = results["windows"] + results["mac_arm"] + results["mac_intel"]
        for x in all_assets:
            download(x.browser_download_url, token)

def main():
    parser = argparse.ArgumentParser(prog='gatherModuleJson', description='Generates module json from a dir containing jasp submodules')
    parser.add_argument('dir', help='Directory containing the JASP submodules')
    parser.add_argument('token', help='GitHub Personal Access Token')
    parser.add_argument('--json', help='Path to the JSON file defining module categories', default='modules.json')
    parser.add_argument('--prerelease', action='store_true', help='Include prerelease assets')
    parser.add_argument('--flatpak', action='store_true', help='Download flatpak assets')
    parser.add_argument('--download', action='store_true', help='Download all non-flatpak assets')
    parser.add_argument('--core', action='store_true', help='Only gather modules listed as "common" in the JSON')

    args = parser.parse_args()
    
    core_modules = []
    if args.core:
        try:
            with open(args.json, 'r') as f:
                module_data = json.load(f)
                # Map the "common" key from JSON to the "core" concept
                core_modules = module_data.get("common", []) 
        except FileNotFoundError:
            print(f"Error: JSON file '{args.json}' not found. Cannot filter by core.", file=sys.stderr)
            sys.exit(1)
        except json.JSONDecodeError as e:
            print(f"Error parsing JSON: {e}", file=sys.stderr)
            sys.exit(1)

    path = Path(args.dir) 
    
    # Helper function to check if a module should be included
    def should_include(module_name):
        if not args.core:
            return True
        return module_name in core_modules

    # Construct module list with filtering
    modules = [
        'jasp-stats-modules/' + y.name 
        for y in path.glob('Official/*') 
        if y.is_dir() and should_include(y.name)
    ]
        
    if not modules:
        print("No modules found matching the criteria.", file=sys.stderr)
        return

    print(f"Gathering modules: {modules}", file=sys.stderr)
    gatherMod(modules, args.token, args.prerelease, args.flatpak, args.download)

if __name__ == "__main__":
    main()
