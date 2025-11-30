import { describe, it, expect, vi } from 'vitest';
import { render, screen } from '@testing-library/svelte';
import userEvent from '@testing-library/user-event';
import Button from './Button.svelte';

describe('Button', () => {
	it('renders with default props', () => {
		render(Button, { ariaLabel: 'Click me', children: () => 'Click me' });
		const button = screen.getByRole('button', { name: 'Click me' });
		expect(button).toBeInTheDocument();
		expect(button).toHaveClass('btn', 'btn-primary', 'btn-md');
		expect(button).not.toBeDisabled();
	});

	it('renders with custom variant', () => {
		render(Button, { variant: 'secondary', ariaLabel: 'Secondary', children: () => 'Secondary' });
		const button = screen.getByRole('button', { name: 'Secondary' });
		expect(button).toHaveClass('btn-secondary');
	});

	it('renders with custom size', () => {
		render(Button, { size: 'lg', ariaLabel: 'Large', children: () => 'Large' });
		const button = screen.getByRole('button', { name: 'Large' });
		expect(button).toHaveClass('btn-lg');
	});

	it('renders as disabled', () => {
		render(Button, { disabled: true, ariaLabel: 'Disabled', children: () => 'Disabled' });
		const button = screen.getByRole('button', { name: 'Disabled' });
		expect(button).toBeDisabled();
		expect(button).toHaveClass('disabled');
	});

	it('renders with aria-label', () => {
		render(Button, { ariaLabel: 'Test button', children: () => 'Test' });
		const button = screen.getByRole('button', { name: 'Test button' });
		expect(button).toHaveAttribute('aria-label', 'Test button');
	});

	it('handles click events', async () => {
		const user = userEvent.setup();
		const mockClick = vi.fn();
		render(Button, { onclick: mockClick, ariaLabel: 'Click', children: () => 'Click' });
		const button = screen.getByRole('button', { name: 'Click' });
		await user.click(button);
		expect(mockClick).toHaveBeenCalledTimes(1);
	});

	it('renders icon variant correctly', () => {
		render(Button, { variant: 'icon', ariaLabel: '+', children: () => '+' });
		const button = screen.getByRole('button', { name: '+' });
		expect(button).toHaveClass('btn-icon');
	});

	it('renders with correct type', () => {
		render(Button, { type: 'submit', ariaLabel: 'Submit', children: () => 'Submit' });
		const button = screen.getByRole('button', { name: 'Submit' });
		expect(button).toHaveAttribute('type', 'submit');
	});
});