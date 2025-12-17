{ pkgs, lib, config, inputs, ... }:

let
  # Playwright browser compatibility for Vitest 3.2.4 + Playwright 1.56
  playwrightBrowsersCompat = pkgs.runCommand "playwright-browsers-compat" {} ''
    mkdir -p $out
    for dir in ${pkgs.playwright-driver.browsers}/*; do
      base="$(basename "$dir")"
      ln -s "$dir" "$out/$base"
    done

    # Vitest 3.2.4 + Playwright 1.56 expect revision 1194, which isn't yet packaged in nixpkgs.
    ln -s ${pkgs.playwright-driver.browsers}/chromium-1181 $out/chromium-1194
    ln -s ${pkgs.playwright-driver.browsers}/chromium_headless_shell-1181 $out/chromium_headless_shell-1194
  '';
in
{
  languages.javascript.enable = true;
  languages.javascript.pnpm.enable = true;

  packages = with pkgs; [
    chromium
    playwright-driver
    playwright-driver.browsers
    playwrightBrowsersCompat
    python3
    nodejs
  ];

  dotenv.disableHint = true;

  # Point Playwright to the Nix-provided browser bundle so Vitest doesn't try to download one.
  env.PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
  env.PLAYWRIGHT_BROWSERS_PATH = "${playwrightBrowsersCompat}";
}
