{ pkgs, lib, config, inputs, ... }:

let
  # Playwright browser compatibility for multiple versions
  # - Vitest 3.2.4 + Playwright 1.56
  # - elm-spec-runner (Playwright 1.11.1) 
  playwrightBrowsersCompat = pkgs.runCommand "playwright-browsers-compat" {} ''
    mkdir -p $out
    for dir in ${pkgs.playwright-driver.browsers}/*; do
      base="$(basename "$dir")"
      ln -s "$dir" "$out/$base"
    done

    # Vitest 3.2.4 + Playwright 1.56 expect revision 1194, which isn't yet packaged in nixpkgs.
    ln -s ${pkgs.playwright-driver.browsers}/chromium-1181 $out/chromium-1194
    ln -s ${pkgs.playwright-driver.browsers}/chromium_headless_shell-1181 $out/chromium_headless_shell-1194
    
    # Playwright 1.57.0 expects firefox-1497, but nixpkgs has firefox-1489
    ln -s ${pkgs.playwright-driver.browsers}/firefox-1489 $out/firefox-1497
    
    # elm-spec-runner expects chromium_headless_shell-1200
    ln -s ${pkgs.playwright-driver.browsers}/chromium_headless_shell-1181 $out/chromium_headless_shell-1200
  '';
in
{
  languages.javascript.enable = true;
  languages.javascript.pnpm.enable = true;

  languages.elm.enable = true;

  languages.python.enable = true;
  languages.python.uv.enable = true;
  languages.python.venv.enable = true;

  packages = with pkgs; [
    chromium
    playwright-driver
    playwright-driver.browsers
    playwrightBrowsersCompat
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
  ];

  dotenv.disableHint = true;

  # Point Playwright to the Nix-provided browser bundle so Vitest doesn't try to download one.
  env.PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
  env.PLAYWRIGHT_BROWSERS_PATH = "${playwrightBrowsersCompat}";

  scripts = {
    elm-build.exec = "elm make src/Main.elm --output=src/elm.js";
    elm-check.exec = "elm-format src/ --validate && elm-review && elm-test";
    elm-spec.exec = "pnpm test:spec";
    elm-spec-headed.exec = "pnpm test:spec:headed";
  };

  processes = {
    reactor.exec = "elm reactor";
  };
}
