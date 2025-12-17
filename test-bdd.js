#!/usr/bin/env node

import { execSync } from 'child_process';

execSync('npx playwright test --config=playwright.config.ts --grep="feature"', { stdio: 'inherit' });
