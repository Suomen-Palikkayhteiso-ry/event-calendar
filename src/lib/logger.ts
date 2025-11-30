/**
 * Simple logging utility for debugging and monitoring
 */

type LogLevel = 'debug' | 'info' | 'warn' | 'error';

interface LogEntry {
	level: LogLevel;
	message: string;
	data?: any;
	timestamp: Date;
}

class Logger {
	private isDevelopment = import.meta.env.DEV;
	private logs: LogEntry[] = [];

	debug(message: string, data?: any) {
		this.log('debug', message, data);
	}

	info(message: string, data?: any) {
		this.log('info', message, data);
	}

	warn(message: string, data?: any) {
		this.log('warn', message, data);
	}

	error(message: string, data?: any) {
		this.log('error', message, data);
	}

	private log(level: LogLevel, message: string, data?: any) {
		const entry: LogEntry = {
			level,
			message,
			data,
			timestamp: new Date()
		};

		this.logs.push(entry);

		// In development, always log to console
		if (this.isDevelopment) {
			console[level](`[${level.toUpperCase()}] ${message}`, data || '');
		} else {
			// In production, only log errors and warnings
			if (level === 'error' || level === 'warn') {
				console[level](`[${level.toUpperCase()}] ${message}`, data || '');
			}
		}

		// Keep only last 100 logs
		if (this.logs.length > 100) {
			this.logs.shift();
		}
	}

	// Get recent logs for debugging
	getRecentLogs(count = 10): LogEntry[] {
		return this.logs.slice(-count);
	}

	// Clear logs
	clear() {
		this.logs = [];
	}
}

export const logger = new Logger();