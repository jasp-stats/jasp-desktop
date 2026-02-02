# JASP QML Documentation

## How to Build
1.  **Prerequisite:** You must have Qt installed (specifically `Qt Documentation Tools`).
2.  **Setup:** Set the `QT_DIR` environment variable to your Qt Kit (e.g., `C:\Qt\6.10.2\mingw_64`).
3.  **Run:**
    ```bash
    python QMLComponents/build_docs.py
    ```

## How to Edit
* **Components:** Edit the `/*! ... */` comments directly in the `.qml` files in `components/JASP/Controls`.
* **Main Page:** Edit `doc/index.qdoc`.
* **Styles:** Edit `doc/style.css`.
