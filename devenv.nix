{ pkgs, lib, config, inputs, ... }:

let
  # Playwright 1.50.1 expects specific browser revisions that may differ from what
  # nixpkgs provides. Create symlinks to map expected revisions to available ones.
  playwrightBrowsersCompat = pkgs.runCommand "playwright-browsers-compat" {} ''
    mkdir -p $out

    # Link all existing browsers
    for dir in ${pkgs.playwright-driver.browsers}/*; do
      base="$(basename "$dir")"
      ln -s "$dir" "$out/$base"
    done

    # Playwright 1.50.1 browser revision mappings
    # Map chromium headless shell revision 1155 to available 1181
    ln -sf ${pkgs.playwright-driver.browsers}/chromium_headless_shell-1181 $out/chromium_headless_shell-1155

    # Map firefox revision 1471 to available 1489
    ln -sf ${pkgs.playwright-driver.browsers}/firefox-1489 $out/firefox-1471

    # Map webkit special revision 2092 to available 2191
    ln -sf ${pkgs.playwright-driver.browsers}/webkit-2191 $out/webkit_ubuntu20.04_x64_special-2092
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
