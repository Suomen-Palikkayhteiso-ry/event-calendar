#!/usr/bin/env bash
# Script to verify Playwright version compatibility between nixpkgs and package.json
# Run with: devenv shell -- ./scripts/check-playwright-compat.sh

set -e

echo "=== Playwright Compatibility Check ==="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if we're in devenv shell
if [ -z "$PLAYWRIGHT_BROWSERS_PATH" ]; then
    echo -e "${RED}Error: PLAYWRIGHT_BROWSERS_PATH not set.${NC}"
    echo "Please run this script inside devenv shell:"
    echo "  devenv shell -- ./scripts/check-playwright-compat.sh"
    exit 1
fi

# Get nixpkgs playwright version
echo "1. Checking nixpkgs playwright-driver version..."
NIX_VERSION=$(nix-instantiate --eval -E '(import <nixpkgs> {}).playwright-driver.version' 2>/dev/null | tr -d '"')
echo "   nixpkgs playwright-driver: $NIX_VERSION"
echo ""

# Get package.json versions
echo "2. Checking package.json versions..."
PKG_PLAYWRIGHT_TEST=$(grep '"@playwright/test"' package.json | sed 's/.*: *"\([^"]*\)".*/\1/' | tr -d '^~')
PKG_PLAYWRIGHT=$(grep '"playwright"' package.json | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/' | tr -d '^~')
echo "   @playwright/test: $PKG_PLAYWRIGHT_TEST"
echo "   playwright: $PKG_PLAYWRIGHT"
echo ""

# Compare versions
echo "3. Version comparison..."
MISMATCH=0

if [ "$NIX_VERSION" != "$PKG_PLAYWRIGHT_TEST" ]; then
    echo -e "   ${YELLOW}Warning: @playwright/test ($PKG_PLAYWRIGHT_TEST) != nixpkgs ($NIX_VERSION)${NC}"
    MISMATCH=1
else
    echo -e "   ${GREEN}✓ @playwright/test matches nixpkgs${NC}"
fi

if [ "$NIX_VERSION" != "$PKG_PLAYWRIGHT" ]; then
    echo -e "   ${YELLOW}Warning: playwright ($PKG_PLAYWRIGHT) != nixpkgs ($NIX_VERSION)${NC}"
    MISMATCH=1
else
    echo -e "   ${GREEN}✓ playwright matches nixpkgs${NC}"
fi
echo ""

# List available browsers
echo "4. Available browser revisions in PLAYWRIGHT_BROWSERS_PATH:"
echo "   Path: $PLAYWRIGHT_BROWSERS_PATH"
ls -1 "$PLAYWRIGHT_BROWSERS_PATH" | while read browser; do
    echo "   - $browser"
done
echo ""

# Test browser launch (quick check)
echo "5. Testing browser availability..."
BROWSER_TEST_RESULT=0

# Try to run a minimal playwright test using pnpm exec
if command -v pnpm &> /dev/null && [ -d "node_modules" ]; then
    # Create a temporary test script in the project directory
    TEMP_TEST="$(pwd)/.pw-compat-test.mjs"
    cat > "$TEMP_TEST" << 'EOF'
import { chromium, firefox, webkit } from 'playwright';

async function testBrowser(browserType, name) {
    try {
        const browser = await browserType.launch({ headless: true });
        await browser.close();
        console.log(`   ✓ ${name}: OK`);
        return true;
    } catch (e) {
        const msg = e.message.split('\n')[0];
        console.log(`   ✗ ${name}: ${msg}`);
        return false;
    }
}

try {
    const results = await Promise.all([
        testBrowser(chromium, 'Chromium'),
        testBrowser(firefox, 'Firefox'),
        testBrowser(webkit, 'WebKit'),
    ]);
    process.exit(results.every(r => r) ? 0 : 1);
} catch (e) {
    console.log(`   Error: ${e.message}`);
    process.exit(1);
}
EOF
    
    node "$TEMP_TEST" && BROWSER_TEST_RESULT=0 || BROWSER_TEST_RESULT=1
    rm -f "$TEMP_TEST"
else
    echo "   node_modules not found, skipping browser launch test"
    echo "   Run 'pnpm install' first"
fi
echo ""

# Summary
echo "=== Summary ==="
if [ $MISMATCH -eq 0 ] && [ $BROWSER_TEST_RESULT -eq 0 ]; then
    echo -e "${GREEN}All checks passed! Playwright is properly configured.${NC}"
    exit 0
else
    if [ $MISMATCH -eq 1 ]; then
        echo -e "${YELLOW}Version mismatch detected.${NC}"
        echo ""
        echo "To fix, update package.json to use exact versions:"
        echo ""
        echo "  \"@playwright/test\": \"$NIX_VERSION\","
        echo "  \"playwright\": \"$NIX_VERSION\","
        echo ""
        echo "Then run: pnpm install"
    fi
    if [ $BROWSER_TEST_RESULT -eq 1 ]; then
        echo -e "${YELLOW}Browser launch test failed.${NC}"
        echo ""
        echo "You may need to add browser revision symlinks in devenv.nix."
        echo "See agents/playwright-setup.md for details."
    fi
    exit 1
fi
