import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/svelte';
import userEvent from '@testing-library/user-event';
import Modal from './Modal.svelte';

describe('Modal', () => {
	it('does not render when open is false', () => {
		render(Modal, { open: false });
		expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
	});

	it('renders when open is true', () => {
		render(Modal, { open: true });
		const modal = screen.getByRole('dialog');
		expect(modal).toBeInTheDocument();
	});

	it('renders with title', () => {
		render(Modal, { open: true, title: 'Test Modal', children: () => 'Content' });
		expect(screen.getByRole('heading', { name: 'Test Modal' })).toBeInTheDocument();
	});

	it('renders close button when title is present', () => {
		render(Modal, { open: true, title: 'Test', children: () => 'Content' });
		const closeButton = screen.getByRole('button', { name: 'Close modal' });
		expect(closeButton).toBeInTheDocument();
	});

	it('renders with correct size classes', () => {
		render(Modal, { open: true, size: 'lg', children: () => 'Content' });
		const modalContent = document.querySelector('.modal-content');
		expect(modalContent).toHaveClass('modal-lg');
	});

	it('has correct ARIA attributes', () => {
		render(Modal, { open: true, title: 'Test' });
		const modal = screen.getByRole('dialog');
		expect(modal).toHaveAttribute('aria-modal', 'true');
		expect(modal).toHaveAttribute('aria-labelledby', 'modal-title');
	});
});