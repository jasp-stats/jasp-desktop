"""Render all AiBridge Mermaid diagrams to SVG using Playwright + Chromium.

Reads .mmd files from the same directory and renders them to .svg.
"""

import os

from playwright.sync_api import sync_playwright

DIR = os.path.dirname(os.path.abspath(__file__))  # Docs/development/aiBridge/

DIAGRAMS = [
    ("01_intro_flow", "Intro Message Flow"),
    ("02_user_message_flow", "User Message Flow"),
    ("03_tool_call_loop", "Tool Call Loop"),
    ("04_clear_chat_flow", "Clear Chat Flow"),
    ("05_buffers_delta_merge", "SSE Buffer Processing and Delta Merge"),
    ("06_two_path_architecture", "Two-Path Architecture (currentSignals)"),
    ("07_overview", "AiBridge System Overview"),
]


def _build_html(title, mermaid):
    return """<!DOCTYPE html>
<html><head><meta charset="utf-8"><style>
  body {{ margin: 0; padding: 20px; background: white; font-family: sans-serif; }}
  h1 {{ font-size: 16px; color: #333; margin: 0 0 16px 0; }}
  .mermaid {{ max-width: 100%; }}
</style></head><body>
<h1>{title}</h1>
<div class="mermaid">
{mermaid}
</div>
<script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
<script>mermaid.initialize({{startOnLoad:true, theme:'default', securityLevel:'loose'}});</script>
</body></html>""".format(title=title, mermaid=mermaid)


def render_all():
    with sync_playwright() as p:
        browser = p.chromium.launch()
        for name, title in DIAGRAMS:
            mmd_path = os.path.join(DIR, f"{name}.mmd")
            if not os.path.exists(mmd_path):
                print(f"  ✗ {name}.mmd not found — skipping")
                continue

            with open(mmd_path, "r", encoding="utf-8") as f:
                mermaid_src = f.read()

            html = _build_html(title=title, mermaid=mermaid_src)
            page = browser.new_page(viewport={"width": 1400, "height": 1000})
            page.set_content(html, wait_until="networkidle")
            # Wait for the SVG to be rendered
            try:
                page.wait_for_selector("svg", timeout=15000)
            except Exception:
                print(f"WARNING: {name} — no SVG found, saving HTML for debug")
                with open(
                    os.path.join(DIR, f"{name}.html"), "w", encoding="utf-8"
                ) as f:
                    f.write(html)
                page.close()
                continue

            svg = page.evaluate("""() => {
                const svg = document.querySelector("svg");
                if (!svg) return null;
                svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
                return svg.outerHTML;
            }""")

            if svg:
                # Fix mermaid's unclosed <br> tags — invalid in SVG XML
                svg = svg.replace("<br>", "<br/>")
                out_path = os.path.join(DIR, f"{name}.svg")
                with open(out_path, "w", encoding="utf-8") as f:
                    f.write(svg)
                print(f"  ✓ {name}.svg")
            else:
                print(f"  ✗ {name} — evaluate returned null")

            page.close()
        browser.close()


if __name__ == "__main__":
    render_all()
    print("\nDone — SVGs in", DIR)
