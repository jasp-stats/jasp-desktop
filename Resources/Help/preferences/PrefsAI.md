AI Settings
=========

The JASP AI Agent is a built-in assistant that can run analyses, inspect data, annotate results, and answer statistical questions — directly inside JASP. It connects to any OpenAI-compatible API and can be shaped through personas, tool capabilities, and system prompts.

These settings let you connect the agent to your AI provider, customize its behavior, and control which JASP features it can access.

All settings persist across sessions.

---

## AI Service

### Enable / Disable
Toggles the entire AI feature on or off. When enabling, a confirmation dialog explains the terms of use and prompts you to back up your work. With AI disabled, all AI-related settings are hidden.

---

## Connection

### Endpoint URL
The chat completions API endpoint. JASP sends requests here using the standard OpenAI-compatible JSON format. Any provider supporting the `/v1/chat/completions` convention should work.

**Example** (DeepSeek):
```
https://api.deepseek.com/v1/chat/completions
```

### API Key
Your authentication key for the AI provider, stored securely in your operating system's credential store. The key is never saved in plain-text preferences.

### Model
The model name sent in API requests. Must be a model available at the configured endpoint.

**Example**: `deepseek-v4-flash`

### Test Connection
Sends a minimal request to verify that the endpoint URL and API key are valid. A green success message or red error message appears next to the button.

---

## Personas

Personas define the AI assistant's role, expertise, and writing style. JASP ships with several read-only system personas, and you can create your own.

Click the **+** tab to create a new persona. The green dot on a tab marks the currently active persona.

### Name
A display name for the persona (e.g., "Alfred the Assistant").

### Avatar
An optional square image shown next to the assistant's messages in the chat window. Accepted formats: PNG, JPG, GIF, SVG.

### Persona Prompt
Free-form instructions that define the persona's behavior, tone, expertise, and any role-specific rules. This text is combined with the **Common System Prompt** (in the Additional Parameters section) to form the complete system message sent to the AI.

### Set as Active
Makes the selected persona the active one used for all chat interactions.

### Duplicate
Creates an editable copy of the selected persona.

### Delete Persona
Removes a user-created persona. System personas cannot be deleted.

### Reset to Default
Restores a modified system persona to its original shipped configuration. This button only appears for system personas.

---

## Enabled Capabilities

The AI agent uses JASP tools to interact with JASP — for example, listing modules, running analyses, reading data, inspecting results, and composing annotated output. The availability of these tools is controlled through capabilities.

### Capabilities
Each capability is a named group of related tools. The checkbox grid shows every capability and its description. Checking a capability enables all the tools it requires; unchecking disables them.

**Important**: tools are shared across capabilities. If two capabilities both need the same tool, the tool stays enabled as long as at least one of those capabilities is checked. Unchecking every capability that needs a given tool will disable that tool.

### Advanced
Expand this section to toggle individual tools directly, regardless of capability groupings. Capability checkboxes update automatically to reflect which capabilities are fully covered by the current tool set. 

On first use, all capabilities are enabled. Clicking a capability will adjust the tool set and may cascade — for example, enabling a broad capability may also auto-enable narrower capabilities that are now fully covered, and disabling a narrow capability may cause a broader one to lose coverage.

---

## Additional Parameters

### Include full tool schemas in request
Controls how tool definitions are sent to the AI in API requests.

- **On** (default): full JSON schemas (including parameter types like `integer`/`boolean`) are placed in the structured `tools` array. Helps models that struggle with type-safety in tool calls, but uses more tokens.
- **Off**: name-only stubs go in the `tools` array, and full schemas are included as a system message instead. Saves tokens and works well with models that don't need explicit type information (e.g., DeepSeek).

### Common System Prompt
A system prompt applied to every request, regardless of which persona is active. This is where you define global behavior rules, context about JASP, and output formatting preferences. The active persona's prompt is appended after this.

### Single chat token limit
Optionally caps the total tokens in a single API request. If the system prompt, conversation, and tools exceed the limit, the request is rejected with an error message. Use this to control costs or stay within provider limits. Approximately 4 characters ≈ 1 token. Default: 256,000.

### Extra parameters (JSON)
A JSON object merged into every API request body. Use this to pass provider-specific parameters that JASP doesn't expose directly — for example, `max_tokens`, `thinking` mode, or `top_p`. The fields `model`, `stream`, `messages`, `tools`, and `text` are protected and will be ignored if included.

**Example**: `{ "max_tokens": 4096, "thinking": { "type": "enabled" } }`

---

## Per-Message Extra Fields

A JSON object merged into every *message* in the API request (not the top-level body). Use this for per-message features like Anthropic-style explicit caching.

**Example**: `{ "cache_control": { "type": "ephemeral" } }`

The fields `role`, `content`, and `text` are protected.

---

## MCP

### Enable MCP server (Model Context Protocol)
Toggles the MCP server on or off. When enabled, JASP exposes an HTTP RPC server that allows external applications to control JASP — including the AI agent.

Use this to integrate JASP with MCP-compatible tools and workflows.

To change the port or bind IP address, see **Advanced Preferences** > **Remote control**.

---

## Reset AI Settings
Restores the endpoint, API key, model, system prompt, token limit, and all other AI settings to their factory defaults. Personas created by the user are removed; system personas are restored to their original shipped configuration.
