import { execSync } from 'child_process';

execSync('npx playwright test --config=playwright-bdd.config.ts --grep="feature"', { stdio: 'inherit' });
