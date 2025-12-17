import { defineConfig } from 'playwright-bdd';

export default defineConfig({
  features: 'tests/features/*.feature',
  steps: 'tests/steps/**/*.ts',
});