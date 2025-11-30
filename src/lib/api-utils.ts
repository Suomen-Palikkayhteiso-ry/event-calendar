/**
 * API error handling utilities
 */

export interface ApiError {
	message: string;
	code?: string;
	status?: number;
}

export class ApiException extends Error {
	public readonly code?: string;
	public readonly status?: number;

	constructor(message: string, code?: string, status?: number) {
		super(message);
		this.name = 'ApiException';
		this.code = code;
		this.status = status;
	}
}

/**
 * Extracts error information from various error types
 */
export function parseApiError(error: unknown): ApiError {
	if (error instanceof ApiException) {
		return {
			message: error.message,
			code: error.code,
			status: error.status
		};
	}

	if (error instanceof Error) {
		return {
			message: error.message
		};
	}

	if (typeof error === 'string') {
		return {
			message: error
		};
	}

	// Handle PocketBase errors
	if (error && typeof error === 'object' && 'message' in error) {
		const err = error as any;
		return {
			message: err.message || 'Unknown error',
			code: err.code,
			status: err.status
		};
	}

	return {
		message: 'An unexpected error occurred'
	};
}

/**
 * Creates a user-friendly error message
 */
export function getUserFriendlyErrorMessage(error: unknown): string {
	const parsed = parseApiError(error);

	// Map common error codes to user-friendly messages
	switch (parsed.code) {
		case 'validation_error':
			return 'Please check your input and try again.';
		case 'unauthorized':
			return 'You are not authorized to perform this action.';
		case 'not_found':
			return 'The requested item was not found.';
		case 'network_error':
			return 'Network error. Please check your connection and try again.';
		default:
			return parsed.message;
	}
}

/**
 * Retry utility for API calls
 */
export async function retryApiCall<T>(
	fn: () => Promise<T>,
	maxRetries = 3,
	delay = 1000
): Promise<T> {
	let lastError: unknown;

	for (let attempt = 1; attempt <= maxRetries; attempt++) {
		try {
			return await fn();
		} catch (error) {
			lastError = error;

			if (attempt === maxRetries) {
				break;
			}

			// Wait before retrying
			await new Promise((resolve) => setTimeout(resolve, delay * attempt));
		}
	}

	throw lastError;
}

/**
 * Checks if an error is retryable
 */
export function isRetryableError(error: unknown): boolean {
	const parsed = parseApiError(error);

	// Retry on network errors or server errors
	return parsed.code === 'network_error' || !!(parsed.status && parsed.status >= 500);
}
