import os
import shutil
import subprocess
import sys

def find_qdoc():
    """
    Locates the qdoc executable.
    Priority:
    1. QT_DIR environment variable.
    2. System PATH (shutil.which).
    """
    # 1. Check QT_DIR environment variable
    qt_dir = os.environ.get("QT_DIR")
    if qt_dir:
        # Check bin/qdoc (Mac/Linux) or bin/qdoc.exe (Windows)
        bin_dir = os.path.join(qt_dir, "bin")
        qdoc_candidates = ["qdoc.exe", "qdoc", "qdoc6.exe", "qdoc6"]
        
        for candidate in qdoc_candidates:
            qdoc_path = os.path.join(bin_dir, candidate)
            if os.path.isfile(qdoc_path):
                return qdoc_path

    # 2. Fallback to system PATH
    qdoc = shutil.which("qdoc") or shutil.which("qdoc6")
    if qdoc:
        return qdoc

    return None

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    # Ensure we are working relative to the script location
    os.chdir(script_dir)
    
    doc_conf = os.path.join("doc", "jasp_qml.qdocconf")
    output_dir = os.path.join("doc", "html_out")
    
    # 1. Locate QDoc
    qdoc_exe = find_qdoc()
    if not qdoc_exe:
        print("Error: qdoc not found. Please set QT_DIR environment variable to your Qt kit location.")
        sys.exit(1)
        
    print(f"Using qdoc: {qdoc_exe}")

    # 2. Clean Output
    if os.path.exists(output_dir):
        print(f"Cleaning output directory: {output_dir}")
        shutil.rmtree(output_dir)
    
    # 3. Run Build
    cmd = [qdoc_exe, doc_conf]
    print(f"Running: {' '.join(cmd)}")
    
    try:
        # Capture stdout/stderr for debugging
        result = subprocess.run(cmd, check=False, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        
        # Always print output
        if result.stdout:
            print(result.stdout)
        if result.stderr:
            print(result.stderr)
            
        if result.returncode != 0:
            print(f"Error: Build failed with exit code {result.returncode}")
            sys.exit(result.returncode)

    except OSError as e:
        print(f"Execution failed: {e}")
        sys.exit(1)

    # 4. Verification
    index_html = os.path.join(output_dir, "index.html")
    if os.path.exists(index_html):
        print("-" * 40)
        print(f"[SUCCESS] Docs generated at: {os.path.abspath(index_html)}")
        print("-" * 40)
    else:
        print("Error: Build finished but index.html was not found.")
        sys.exit(1)

if __name__ == "__main__":
    main()
