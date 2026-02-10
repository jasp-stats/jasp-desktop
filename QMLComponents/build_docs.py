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

    # 4. Conversion to Markdown (if pandoc is available)
    pandoc_exe = shutil.which("pandoc")
    if pandoc_exe:
        print("-" * 40)
        print(f"Pandoc found: {pandoc_exe}")
        
        md_output_dir = os.path.join("doc", "md_out")
        if os.path.exists(md_output_dir):
            shutil.rmtree(md_output_dir)
        os.makedirs(md_output_dir)
        
        print(f"Converting HTML to Markdown in: {md_output_dir}")
        
        html_files = []
        for root, dirs, files in os.walk(output_dir):
            for file in files:
                if file.endswith(".html"):
                    html_files.append(os.path.join(root, file))
        
        count = 0
        for html_file in html_files:
            rel_path = os.path.relpath(html_file, output_dir)
            md_filename = os.path.splitext(rel_path)[0] + ".md"
            md_file = os.path.join(md_output_dir, md_filename)
            
            # Ensure subdirectories exist
            os.makedirs(os.path.dirname(md_file), exist_ok=True)
            
            # Convert
            # from html to gfm (GitHub Flavored Markdown)
            cmd_pandoc = [pandoc_exe, html_file, "-f", "html", "-t", "gfm", "-o", md_file]
            
            try:
                subprocess.run(cmd_pandoc, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                count += 1
                
                # Fix links: replace .html with .md in the generated file
                with open(md_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                
                if ".html" in content:
                    content = content.replace(".html", ".md")
                    with open(md_file, 'w', encoding='utf-8') as f:
                        f.write(content)

            except subprocess.CalledProcessError:
                print(f"Failed to convert: {html_file}")

        print(f"Converted {count} files to Markdown.")
        print("-" * 40)
        print(f"[SUCCESS] Markdown docs generated at: {os.path.abspath(md_output_dir)}")
    else:
        print("Pandoc not found. Skipping Markdown conversion.")

    # 5. Verification (HTML)
    index_html = os.path.join(output_dir, "index.html")
    if os.path.exists(index_html):
        print("-" * 40)
        print(f"[SUCCESS] HTML Docs generated at: {os.path.abspath(index_html)}")
        print("-" * 40)
    else:
        print("Error: Build finished but index.html was not found.")
        sys.exit(1)

if __name__ == "__main__":
    main()
