#!/usr/bin/env node

/**
 * Database export script for PocketBase
 * Exports production database to JSON fixtures
 */

import https from 'https';
import http from 'http';
import fs from 'fs';
import path from 'path';

async function makeRequest(url, options = {}) {
  return new Promise((resolve, reject) => {
    const client = url.protocol === 'https:' ? https : http;
    const reqOptions = {
      headers: options.headers || {}
    };

    const req = client.get(url, reqOptions, (res) => {
      let data = '';
      res.on('data', (chunk) => {
        data += chunk;
      });
      res.on('end', () => {
        if (res.statusCode >= 200 && res.statusCode < 300) {
          try {
            const json = JSON.parse(data);
            resolve(json);
          } catch (e) {
            resolve(data); // Return raw data if not JSON
          }
        } else {
          reject(new Error(`Request failed with status ${res.statusCode}: ${data}`));
        }
      });
    });

    req.on('error', (err) => {
      reject(err);
    });

    req.setTimeout(30000, () => {
      req.destroy();
      reject(new Error('Request timeout'));
    });
  });
}

async function exportCollection(baseUrl, collection, token) {
  console.log(`Exporting collection: ${collection}`);
  const url = new URL(`/api/collections/${collection}/records`, baseUrl);

  // Add query parameters for all records
  url.searchParams.set('perPage', '1000'); // Get up to 1000 records
  url.searchParams.set('sort', 'created'); // Sort by creation date

  const headers = token ? { 'Authorization': `Bearer ${token}` } : {};

  try {
    const data = await makeRequest(url, { headers });
    return data.items || [];
  } catch (error) {
    console.error(`Failed to export ${collection}:`, error.message);
    return [];
  }
}

async function exportDatabase(baseUrl, token) {
  console.log(`Exporting database from ${baseUrl}...`);

  const collections = ['events', 'users'];
  const exportData = {
    exported_at: new Date().toISOString(),
    source: baseUrl,
    collections: {}
  };

  for (const collection of collections) {
    const records = await exportCollection(baseUrl, collection, token);
    exportData.collections[collection] = records;
    console.log(`Exported ${records.length} records from ${collection}`);
  }

  return exportData;
}

async function saveExport(data, outputDir = './test-db/fixtures') {
  // Ensure output directory exists
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const filename = `production-export-${timestamp}.json`;
  const filepath = path.join(outputDir, filename);

  fs.writeFileSync(filepath, JSON.stringify(data, null, 2));
  console.log(`Export saved to: ${filepath}`);

  // Also save as latest.json for easy reference
  const latestPath = path.join(outputDir, 'latest.json');
  fs.writeFileSync(latestPath, JSON.stringify(data, null, 2));
  console.log(`Latest export saved to: ${latestPath}`);

  return filepath;
}

async function main() {
  const baseUrl = process.env.POCKETBASE_URL || 'https://data.suomenpalikkayhteiso.fi';
  const token = process.env.POCKETBASE_TOKEN; // Optional admin token
  const outputDir = process.env.EXPORT_DIR || './test-db/fixtures';

  console.log(`Starting database export...`);
  console.log(`Source: ${baseUrl}`);
  console.log(`Output: ${outputDir}`);

  try {
    const exportData = await exportDatabase(baseUrl, token);
    const filepath = await saveExport(exportData, outputDir);

    console.log('🎉 Database export completed successfully!');
    console.log(`Total collections: ${Object.keys(exportData.collections).length}`);
    console.log(`Total records: ${Object.values(exportData.collections).reduce((sum, records) => sum + records.length, 0)}`);

    // Print summary
    Object.entries(exportData.collections).forEach(([collection, records]) => {
      console.log(`  ${collection}: ${records.length} records`);
    });

  } catch (error) {
    console.error('❌ Database export failed:', error.message);
    process.exit(1);
  }
}

// Run main if this is the main module
main();