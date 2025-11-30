import { describe, it, expect, vi } from 'vitest';
import { render, screen } from '@testing-library/svelte';
import userEvent from '@testing-library/user-event';
import Input from './Input.svelte';

describe('Input', () => {
	it('renders with default props', () => {
		render(Input);
		const input = screen.getByRole('textbox');
		expect(input).toBeInTheDocument();
		expect(input).toHaveAttribute('type', 'text');
		expect(input).toHaveClass('form-input');
	});

	it('renders with custom type', () => {
		render(Input, { type: 'email' });
		const input = screen.getByRole('textbox');
		expect(input).toHaveAttribute('type', 'email');
	});

	it('renders with placeholder', () => {
		render(Input, { placeholder: 'Enter text' });
		const input = screen.getByPlaceholderText('Enter text');
		expect(input).toBeInTheDocument();
	});

	it('renders as required', () => {
		render(Input, { required: true });
		const input = screen.getByRole('textbox');
		expect(input).toHaveAttribute('required');
	});

	it('renders as disabled', () => {
		render(Input, { disabled: true });
		const input = screen.getByRole('textbox');
		expect(input).toBeDisabled();
	});

	it('renders with aria-label', () => {
		render(Input, { ariaLabel: 'Test input' });
		const input = screen.getByRole('textbox', { name: 'Test input' });
		expect(input).toHaveAttribute('aria-label', 'Test input');
	});

	it('binds value correctly', async () => {
		const user = userEvent.setup();
		render(Input, { value: 'initial' });
		const input = screen.getByRole('textbox');
		expect(input).toHaveValue('initial');
		await user.clear(input);
		await user.type(input, 'new value');
		expect(input).toHaveValue('new value');
	});

	it('dispatches input event', async () => {
		const user = userEvent.setup();
		const mockInput = vi.fn();
		render(Input, { value: 'test' });
		const input = screen.getByRole('textbox');
		input.addEventListener('input', mockInput);
		await user.type(input, 'a');
		expect(mockInput).toHaveBeenCalled();
	});

	it('renders with custom class', () => {
		render(Input, { class: 'custom-class' });
		const input = screen.getByRole('textbox');
		expect(input).toHaveClass('form-input', 'custom-class');
	});
});