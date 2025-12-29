import { execSync } from 'child_process';

execSync('npx playwright test', {
	stdio: 'inherit'
});
