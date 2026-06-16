"""Render all AiBridge Mermaid diagrams to SVG using Playwright + Chromium."""

import os
import sys

from playwright.sync_api import sync_playwright

DIR = os.path.dirname(os.path.abspath(__file__))  # Docs/development/aiBridge/

DIAGRAMS = {
    "01_intro_flow": {
        "title": "Intro Message Flow",
        "mermaid": """sequenceDiagram
    participant WC as QWebChannel
    participant JS as chat-bridge.js
    participant CPP as AiBridge (C++)
    participant API as LLM API
    participant DC as deep-chat

    rect rgb(220, 235, 255)
        Note over CPP,WC: INITIALIZATION (page load)
        CPP->>CPP: connect(personaChanged → clearChat)
        JS->>WC: DOMContentLoaded → create QWebChannel
        WC-->>JS: expose aiBridge object
        JS->>JS: setupDeepChat()
        JS->>CPP: setTimeout: clearChat() [100ms]
    end

    rect rgb(255, 250, 230)
        Note over CPP,DC: INTRO MESSAGE FLOW
        CPP->>CPP: stopStream()
        CPP->>CPP: clearConversation()
        CPP-->>JS: emit onClearChat()
        JS->>JS: currentSignals = null
        JS->>DC: clearMessages()
        JS->>DC: addMessage("Starting up…")
        CPP->>CPP: sendIntroMessage()
        CPP-->>JS: emit onStreamOpen()
        JS->>JS: _introBuf = ""
        CPP->>API: POST /chat/completions (SSE)
        loop Each SSE chunk
            API-->>CPP: delta: {"content":"Hi!"}
            CPP-->>JS: emit onStreamChunk("Hi!")
            JS->>JS: _introBuf += "Hi!"
        end
        API-->>CPP: data: [DONE]
        CPP-->>JS: emit onStreamClose()
        JS->>DC: clearMessages()
        JS->>DC: addMessage(_introBuf)
        Note over JS,DC: "Starting up…" replaced
    end""",
    },
    "02_user_message_flow": {
        "title": "User Message Flow",
        "mermaid": """sequenceDiagram
    participant User
    participant DC as deep-chat
    participant JS as chat-bridge.js
    participant CPP as AiBridge (C++)
    participant API as LLM API

    rect rgb(230, 255, 230)
        Note over User,API: USER SENDS A MESSAGE
        User->>DC: Types "Run a t-test", clicks send
        DC->>JS: connect.handler(messages, signals)
        JS->>JS: currentSignals = signals
        JS->>CPP: aiBridge.startStream(json)
        CPP->>CPP: append user messages
        CPP-->>JS: emit onStreamOpen()
        JS->>DC: signals.onOpen()
        Note over DC: new AI message bubble
        CPP->>API: POST /chat/completions (SSE)
        Note over CPP,API: body: model, messages, tools
        loop SSE streaming
            API-->>CPP: delta: {"content":"Sure"}
            CPP-->>JS: onStreamChunk("Sure")
            JS->>DC: signals.onResponse({text:"Sure"})
            API-->>CPP: delta: {"content":", running now"}
            CPP-->>JS: onStreamChunk(", running now")
            JS->>DC: signals.onResponse({text:", running now"})
        end
        API-->>CPP: data: [DONE]
        CPP-->>JS: emit onStreamClose()
        JS->>DC: signals.onClose()
        CPP->>CPP: save assistant msg to m_conversation
    end""",
    },
    "03_tool_call_loop": {
        "title": "Tool Call Loop",
        "mermaid": """sequenceDiagram
    participant API as LLM API
    participant CPP as AiBridge (C++)
    participant RPC as JaspRpcDispatcher
    participant JS as chat-bridge.js
    participant DC as deep-chat

    rect rgb(255, 230, 230)
        Note over API,DC: TOOL CALL LOOP
        API-->>CPP: delta.tool_calls: [{function:{name:"createAnalysis"}}]
        loop Fragment accumulation
            CPP->>CPP: processToolCalls()
            Note over CPP: accumulate by index in m_toolCallAccum
            API-->>CPP: delta.tool_calls: [{function:{arguments:"..."}}]
        end
        API-->>CPP: data: [DONE]
        CPP->>CPP: flushToolCalls()
        CPP->>RPC: dispatch("createAnalysis", params)
        RPC-->>CPP: result JSON
        CPP->>CPP: append tool result to m_conversation
        CPP-->>JS: emit onStreamOpen()
        JS->>DC: signals.onOpen() (new bubble)
        CPP->>API: POST /chat/completions (with tool results)
        Note over API,CPP: Loop continues until no more tool calls
        API-->>CPP: delta: {"content":"Done! Analysis created."}
        CPP-->>JS: onStreamChunk("Done! Analysis created.")
        JS->>DC: signals.onResponse({text:"Done!"})
        API-->>CPP: data: [DONE]
        CPP-->>JS: emit onStreamClose()
        JS->>DC: signals.onClose()
        CPP->>CPP: save assistant msg to m_conversation
    end""",
    },
    "04_clear_chat_flow": {
        "title": "Clear Chat Flow",
        "mermaid": """sequenceDiagram
    participant User
    participant DC as deep-chat
    participant JS as chat-bridge.js
    participant CPP as AiBridge (C++)

    rect rgb(255, 255, 220)
        Note over User,CPP: USER RESETS OR PERSONA SWITCHES
        User->>DC: Clicks reset button
        DC->>JS: aiBridgeInterface.clearChat()
        JS->>CPP: aiBridge.clearChat()
        CPP->>CPP: stopStream()
        Note over CPP: abort active HTTP reply
        CPP->>CPP: clearConversation()
        Note over CPP: m_conversation = []
        Note over CPP: m_assistantDelta = {}
        Note over CPP: m_totalInputTokens = 0
        CPP-->>JS: emit onClearChat()
        JS->>JS: currentSignals = null
        JS->>DC: clearMessages()
        JS->>DC: addMessage("Starting up…")
        CPP->>CPP: sendIntroMessage()
        Note over CPP,DC: → triggers INTRO MESSAGE FLOW
    end""",
    },
    "05_buffers_delta_merge": {
        "title": "SSE Buffer Processing and Delta Merge",
        "mermaid": """flowchart TD
    NET["Network: TCP bytes arrive"]
    SB["m_sseBuffer += bytes"]
    SL["Slice by newline into lines"]
    PSL["processSSELine(line)"]
    TC{"line starts with 'data:'?"}
    DONE{"data == '[DONE]'?"}
    JSON["Parse JSON"]
    DELTA["Extract delta = choices[0].delta"]
    TCC{"delta.tool_calls?"}
    PT["processToolCalls()\n→ accumulate in m_toolCallAccum"]
    LOOP["Iterate ALL keys in delta"]
    CONTENT{"key == 'content'?"}
    EMIT["emit onStreamChunk(text)\n→ user sees text"]
    STRING{"val is string\nand key exists?"}
    CONCAT["concatenate to\nm_assistantDelta[key]"]
    OVERWRITE["set m_assistantDelta[key] = val"]
    NULL{"val is null?"}
    SKIP["skip (don't clear)"]
    MORE{"more lines?"}
    STREAM_END["Stream ends → onReplyFinished()"]
    SAVE["Save m_assistantDelta\nas assistant msg"]
    TOOLS{"m_pendingToolCalls\nnot empty?"}
    FLUSH["flushToolCalls()\n→ dispatch via RPC"]
    CONTINUE["Append tool results\nto m_conversation"]
    LOOPBACK["sendToAI() again"]

    NET --> SB --> SL --> PSL --> TC
    TC -->|no| SL
    TC -->|yes| DONE
    DONE -->|yes| STREAM_END
    DONE -->|no| JSON --> DELTA --> TCC
    TCC -->|yes| PT
    TCC -->|no| LOOP
    LOOP --> CONTENT
    CONTENT -->|yes| EMIT
    CONTENT -->|no| STRING
    EMIT --> STRING
    STRING -->|yes| CONCAT
    STRING -->|no| NULL
    NULL -->|yes| SKIP
    NULL -->|no| OVERWRITE
    CONCAT --> LOOP
    SKIP --> LOOP
    OVERWRITE --> LOOP
    LOOP -->|more keys| LOOP
    LOOP -->|done| MORE
    MORE -->|yes| SL
    MORE -->|no| NET
    STREAM_END --> SAVE --> TOOLS
    TOOLS -->|yes| FLUSH --> CONTINUE --> LOOPBACK
    TOOLS -->|no| DONE_STATE(["Stream complete"])
    LOOPBACK --> NET""",
    },
    "06_two_path_architecture": {
        "title": "Two-Path Architecture (currentSignals)",
        "mermaid": """flowchart TD
    ON_OPEN["onStreamOpen()"]
    ON_CHUNK["onStreamChunk(text)"]
    ON_CLOSE["onStreamClose()"]
    CS{"currentSignals\n== null ?"}

    OP_NULL["_introBuf = ''"]
    OP_SIG["signals.onOpen()"]
    OP_NEEDLE{"streamHasContent?"}
    OP_NL["signals._needNewline = true"]

    CH_NULL["_introBuf += text"]
    CH_SIG_NL{"_needNewline?"}
    CH_SIG_PRE["text = '\\n' + text"]
    CH_SIG["signals.onResponse({text})"]
    CHASH["streamHasContent = true"]

    CL_NULL1["chat = querySelector('deep-chat')"]
    CL_NULL2["chat.clearMessages()"]
    CL_NULL3{"_introBuf ?"}
    CL_NULL4["chat.addMessage({text: _introBuf, role: 'ai'})"]
    CL_NULL5["_introBuf = ''"]
    CL_SIG["signals.onClose()"]
    CL_COMMENT["Keep currentSignals alive\nfor tool-call loops"]

    ON_OPEN --> CS
    CS -->|"yes (intro path)"| OP_NULL
    CS -->|"no (user path)"| OP_SIG --> OP_NEEDLE
    OP_NEEDLE -->|yes| OP_NL --> DONE(["done"])
    OP_NEEDLE -->|no| DONE

    ON_CHUNK --> CS
    CS -->|"yes (intro path)"| CH_NULL --> CHASH
    CS -->|"no (user path)"| CH_SIG_NL
    CH_SIG_NL -->|yes| CH_SIG_PRE --> CH_SIG --> CHASH
    CH_SIG_NL -->|no| CH_SIG --> CHASH

    ON_CLOSE --> CS
    CS -->|"yes (intro path)"| CL_NULL1 --> CL_NULL2 --> CL_NULL3
    CL_NULL3 -->|yes| CL_NULL4 --> CL_NULL5 --> CL_COMMENT
    CL_NULL3 -->|no| CL_NULL5 --> CL_COMMENT
    CS -->|"no (user path)"| CL_SIG --> CL_COMMENT""",
    },
    "07_overview": {
        "title": "AiBridge System Overview",
        "mermaid": """flowchart LR
    subgraph QML["ChatWindow.qml"]
        WV["WebEngineView"]
        CB["Control Bar\n(reset, save, tokens)"]
    end

    subgraph HTML["chat.html"]
        DC["deep-chat component"]
    end

    subgraph JS["chat-bridge.js"]
        SIG["currentSignals\n(routing switch)"]
        IBUF["_introBuf\n(intro accumulator)"]
    end

    subgraph CPP["aiBridge.cpp"]
        CONV["m_conversation\n(history)"]
        SSE["m_sseBuffer\n(raw bytes)"]
        DELTA["m_assistantDelta\n(response acc)"]
        TOOLS["m_toolCallAccum\n(tool fragments)"]
    end

    subgraph EXT["External"]
        API["OpenAI-compatible API"]
        RPC["JaspRpcDispatcher"]
    end

    WV -->|loads| HTML
    CB -->|clearChat| CPP
    DC -->|user message| SIG
    DC -->|render| DC
    SIG -->|startStream| CPP
    CPP -->|onStreamChunk| SIG
    CPP -->|onClearChat| SIG
    CPP -->|onStreamClose| SIG
    SIG -->|onResponse| DC
    SIG -.->|null: intro path| IBUF
    IBUF -->|addMessage| DC
    CPP --> API
    CPP --> RPC
    CONV -->|buildRequestBody| API""",
    },
}


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
        for name, info in DIAGRAMS.items():
            html = _build_html(title=info["title"], mermaid=info["mermaid"])
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
