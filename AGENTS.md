AGENTS.md is an open format designed to guide AI coding agents, functioning similarly to a README file but specifically tailored for agents.[1] It provides context and instructions to help AI coding agents work on a project and is used by over 20,000 open-source projects.[1]

Key aspects of AGENTS.md include:

- **Purpose**: It offers a dedicated and predictable place for build steps, tests, and conventions that might otherwise clutter a `README.md` file or be irrelevant to human contributors.[1]
- **Content**: It can include sections for setup commands (e.g., `pnpm install`, `pnpm dev`, `pnpm test`), code style guidelines (e.g., TypeScript strict mode, single quotes, functional patterns), and testing instructions.[1]
- **Usage**: To use it, you create an `AGENTS.md` file at the root of your repository and add sections that help an agent work effectively with your project, such as project overview, build/test commands, code style, and security considerations.[1]
- **Monorepos**: For large monorepos, nested `AGENTS.md` files can be used for subprojects, with the nearest file in the directory tree taking precedence.[1]
- **Compatibility**: It is compatible with a growing ecosystem of AI coding agents and tools, including Semgrep, Ona, GitHub Copilot, Jules from Google, Kilo Code, Factory, Phoenix, Coded Agents from UiPath, Aider, Gemini CLI from Google, Zed, Codex from OpenAI, RooCode, Cursor, Warp, opencode, Amp, VS Code, and Devin from Cognition.[1]

Sources:
[1] AGENTS.md (https://agents.md/)
