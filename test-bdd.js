import { execSync } from 'child_process';

// Generate BDD tests from features
execSync('npx bddgen', {
	stdio: 'inherit'
});

// Run the tests
execSync('npx playwright test', {
	stdio: 'inherit'
});
