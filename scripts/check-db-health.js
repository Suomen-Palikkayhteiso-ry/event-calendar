#!/usr/bin/env node

/**
 * Database health check script for PocketBase
 * Verifies connectivity and data integrity
 */

import https from 'https';
import http from 'http';

async function checkHealth(baseUrl) {
  console.log(`Making health check request to ${baseUrl}/api/health`);
  return new Promise((resolve, reject) => {
    try {
      const url = new URL('/api/health', baseUrl);
      console.log(`Parsed URL: ${url.href}`);
      const client = url.protocol === 'https:' ? https : http;

      const req = client.get(url, (res) => {
      let data = '';
      res.on('data', (chunk) => {
        data += chunk;
      });
      res.on('end', () => {
        if (res.statusCode === 200) {
          try {
            const health = JSON.parse(data);
            resolve(health);
          } catch (e) {
            resolve({ status: 'ok' }); // Some PocketBase versions return simple text
          }
        } else {
          reject(new Error(`Health check failed with status ${res.statusCode}`));
        }
      });
    });

    req.on('error', (err) => {
      reject(err);
    });

    req.setTimeout(5000, () => {
      req.destroy();
      reject(new Error('Health check timeout'));
    });
    } catch (err) {
      reject(new Error(`Invalid URL: ${err.message}`));
    }
  });
}

async function checkCollections(baseUrl, token) {
  return new Promise((resolve, reject) => {
    const url = new URL('/api/collections', baseUrl);
    const client = url.protocol === 'https:' ? https : http;

    const options = {
      headers: token ? { 'Authorization': `Bearer ${token}` } : {}
    };

    const req = client.get(url, options, (res) => {
      let data = '';
      res.on('data', (chunk) => {
        data += chunk;
      });
      res.on('end', () => {
        if (res.statusCode === 200) {
          try {
            const collections = JSON.parse(data);
            resolve(collections);
          } catch (e) {
            reject(new Error('Failed to parse collections response'));
          }
        } else {
          reject(new Error(`Collections check failed with status ${res.statusCode}`));
        }
      });
    });

    req.on('error', (err) => {
      reject(err);
    });
  });
}

async function checkEvents(baseUrl, token) {
  return new Promise((resolve, reject) => {
    const url = new URL('/api/collections/events/records', baseUrl);
    const client = url.protocol === 'https:' ? https : http;

    const options = {
      headers: token ? { 'Authorization': `Bearer ${token}` } : {}
    };

    const req = client.get(url, options, (res) => {
      let data = '';
      res.on('data', (chunk) => {
        data += chunk;
      });
      res.on('end', () => {
        if (res.statusCode === 200) {
          try {
            const events = JSON.parse(data);
            resolve(events);
          } catch (e) {
            reject(new Error('Failed to parse events response'));
          }
        } else {
          reject(new Error(`Events check failed with status ${res.statusCode}`));
        }
      });
    });

    req.on('error', (err) => {
      reject(err);
    });
  });
}

async function validateTestData(events) {
  const issues = [];

  if (!events || !Array.isArray(events.items)) {
    issues.push('Events collection is not accessible or malformed');
    return issues;
  }

  const testEvents = events.items.filter(event =>
    event.title && event.title.startsWith('Test Event')
  );

  if (testEvents.length === 0) {
    issues.push('No test events found in database');
  } else {
    console.log(`Found ${testEvents.length} test events`);

    // Check for future dates
    const now = new Date();
    const futureEvents = testEvents.filter(event => {
      const eventDate = new Date(event.start_date);
      return eventDate > now;
    });

    if (futureEvents.length === 0) {
      issues.push('No test events have future dates');
    } else {
      console.log(`${futureEvents.length} test events have future dates`);
    }
  }

  return issues;
}

async function main() {
  const baseUrl = process.env.POCKETBASE_URL || 'http://127.0.0.1:8090';
  const token = process.env.POCKETBASE_TOKEN; // Optional admin token

  console.log(`Checking database health at ${baseUrl}...`);

  try {
    // Check health endpoint
    console.log('Checking health endpoint...');
    const health = await checkHealth(baseUrl);
    console.log('✅ Health check passed');

    // Check collections
    console.log('Checking collections...');
    const collections = await checkCollections(baseUrl, token);
    console.log(`✅ Found ${collections.length} collections`);

    // Check events collection exists
    const eventsCollection = collections.find(c => c.name === 'events');
    if (!eventsCollection) {
      throw new Error('Events collection not found');
    }
    console.log('✅ Events collection exists');

    // Check events data
    console.log('Checking events data...');
    const events = await checkEvents(baseUrl, token);
    console.log(`✅ Found ${events.items?.length || 0} events`);

    // Validate test data
    console.log('Validating test data...');
    const issues = await validateTestData(events);

    if (issues.length > 0) {
      console.log('⚠️  Data validation issues:');
      issues.forEach(issue => console.log(`  - ${issue}`));
    } else {
      console.log('✅ Test data validation passed');
    }

    console.log('🎉 Database health check completed successfully!');
    process.exit(0);

  } catch (error) {
    console.error('❌ Database health check failed:', error.message);
    process.exit(1);
  }
}

// Run main if this is the main module
main();

export { checkHealth, checkCollections, checkEvents, validateTestData };