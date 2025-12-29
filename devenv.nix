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
  languages.haskell.package = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
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
    feed
  ]);

  dotenv.disableHint = true;

  # PocketBase for local testing
  packages = [
    pkgs.pocketbase
    pkgs.treefmt
  ];

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
    pocketbase-init.exec = "mkdir -p .pocketbase && pocketbase migrate";
    pocketbase-serve.exec = "pocketbase serve --dir=.pocketbase --http=127.0.0.1:8090";
    pocketbase-admin.exec = "pocketbase admin --dir=.pocketbase";
    check-db-health.exec = "pnpm check-db-health";
    export-db.exec = "pnpm export-db";
    import-test-db.exec = "pnpm import-test-db";
    test-db-setup.exec = "./scripts/test-db/setup-test-db.sh";
    test-local.exec = "POCKETBASE_URL=http://127.0.0.1:8090 make test-e2e";
    test.exec = "devenv run test-db-setup && devenv run check-db-health && devenv run test-local";
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
