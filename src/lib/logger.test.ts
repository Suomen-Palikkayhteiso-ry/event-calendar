import { describe, it, expect, vi, beforeEach } from 'vitest';
import { logger } from '$lib/logger';

describe('logger', () => {
	beforeEach(() => {
		vi.clearAllMocks();
		// Clear logs
		logger.clear();
	});

	it('should log debug messages', () => {
		const consoleSpy = vi.spyOn(console, 'debug').mockImplementation(() => {});
		logger.debug('Debug message', { data: 'test' });
		expect(consoleSpy).toHaveBeenCalledWith('[DEBUG] Debug message', { data: 'test' });
		consoleSpy.mockRestore();
	});

	it('should log info messages', () => {
		const consoleSpy = vi.spyOn(console, 'info').mockImplementation(() => {});
		logger.info('Info message', { data: 'test' });
		expect(consoleSpy).toHaveBeenCalledWith('[INFO] Info message', { data: 'test' });
		consoleSpy.mockRestore();
	});

	it('should log warn messages', () => {
		const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
		logger.warn('Warn message', { data: 'test' });
		expect(consoleSpy).toHaveBeenCalledWith('[WARN] Warn message', { data: 'test' });
		consoleSpy.mockRestore();
	});

	it('should log error messages', () => {
		const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
		logger.error('Error message', { data: 'test' });
		expect(consoleSpy).toHaveBeenCalledWith('[ERROR] Error message', { data: 'test' });
		consoleSpy.mockRestore();
	});

	it('should store logs', () => {
		logger.info('Test message');
		const logs = logger.getRecentLogs();
		expect(logs).toHaveLength(1);
		expect(logs[0].message).toBe('Test message');
		expect(logs[0].level).toBe('info');
	});

	it('should limit logs to 100', () => {
		for (let i = 0; i < 105; i++) {
			logger.info(`Message ${i}`);
		}
		const logs = logger.getRecentLogs(105);
		expect(logs).toHaveLength(100);
	});

	it('should return recent logs with limit', () => {
		logger.info('Message 1');
		logger.info('Message 2');
		logger.info('Message 3');
		const logs = logger.getRecentLogs(2);
		expect(logs).toHaveLength(2);
		expect(logs[0].message).toBe('Message 2');
		expect(logs[1].message).toBe('Message 3');
	});

	it('should clear logs', () => {
		logger.info('Test message');
		expect(logger.getRecentLogs()).toHaveLength(1);
		logger.clear();
		expect(logger.getRecentLogs()).toHaveLength(0);
	});
});