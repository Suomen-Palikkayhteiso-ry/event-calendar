{ pkgs, lib, config, inputs, ... }:

let
  browsers = (builtins.fromJSON (builtins.readFile "${pkgs.playwright-driver}/browsers.json")).browsers;
  chromium-rev = (builtins.head (builtins.filter (x: x.name == "chromium") browsers)).revision;
in
{
  languages.javascript.enable = true;
  languages.javascript.pnpm.enable = true;

  languages.elm.enable = true;

  languages.python.enable = true;
  languages.python.uv.enable = true;
  languages.python.venv.enable = true;

  languages.haskell.enable = true;

  packages = with pkgs; [
    chromium
    python3
    nodejs
    elmPackages.elm-review
    elmPackages.elm-format
    elmPackages.elm-test
    elmPackages.elm-language-server
    (python3.withPackages (ps: with ps; [
      requests
      feedgen
      icalendar
      qrcode
      pillow
    ]))
    (haskellPackages.ghcWithPackages (ps: with ps; [
      aeson
      http-client
      http-client-tls
      time
      directory
      filepath
      text
      bytestring
      containers
      vector
      unordered-containers
      scientific
      uuid
      base64-bytestring
    ]))
  ];

  dotenv.disableHint = true;

  # https://devenv.sh/basics/
  env = {
    PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright.browsers}";
    PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = true;
    PLAYWRIGHT_NODEJS_PATH = "${pkgs.nodejs}/bin/node";
    PLAYWRIGHT_LAUNCH_OPTIONS_EXECUTABLE_PATH = "${pkgs.playwright.browsers}/chromium-${chromium-rev}/chrome-linux/chrome";
  };

  scripts = {
    elm-build.exec = "elm make src/Main.elm --output=src/elm.js";
    elm-check.exec = "elm-format src/ --validate && elm-review && elm-test";
    elm-spec.exec = "pnpm test:spec";
    elm-spec-headed.exec = "pnpm test:spec:headed";
    intro.exec = ''
      playwrightNpmVersion="$(grep '"@playwright/test"' package.json | sed 's/.*"\\([^"]*\\)".*/\\1/')"
      echo "❄️ Playwright nix version: ${pkgs.playwright.version}"
      echo "📦 Playwright npm version: $playwrightNpmVersion"

      if [ "${pkgs.playwright.version}" != "$playwrightNpmVersion" ]; then
          echo "❌ Playwright versions in nix and npm are not the same! Please adapt the configuration."
      else
          echo "✅ Playwright versions in nix and npm are the same"
      fi

      echo
      env | grep ^PLAYWRIGHT
    '';
  };

  enterShell = ''
    intro
  '';
}
