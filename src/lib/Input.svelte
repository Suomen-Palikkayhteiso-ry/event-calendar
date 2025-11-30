<script lang="ts">
	import { createEventDispatcher } from 'svelte';

	interface Props {
		type?: string;
		value?: string | number;
		placeholder?: string;
		required?: boolean;
		disabled?: boolean;
		readonly?: boolean;
		ariaLabel?: string;
		ariaDescribedBy?: string;
		ariaInvalid?: boolean;
		id?: string;
		name?: string;
		pattern?: string;
		min?: number | string;
		max?: number | string;
		step?: number | string;
		accept?: string;
		children?: any;
	}

	let {
		type = 'text',
		value = '',
		placeholder,
		required = false,
		disabled = false,
		readonly = false,
		ariaLabel,
		ariaDescribedBy,
		ariaInvalid = false,
		id,
		name,
		pattern,
		min,
		max,
		step,
		accept,
		children
	}: Props = $props();

	const dispatch = createEventDispatcher();
</script>

<input
	{type}
	bind:value
	{placeholder}
	{required}
	{disabled}
	{readonly}
	aria-label={ariaLabel}
	aria-describedby={ariaDescribedBy}
	aria-invalid={ariaInvalid}
	{id}
	{name}
	{pattern}
	{min}
	{max}
	{step}
	{accept}
	class="form-input"
	oninput={(e) => dispatch('input', e)}
	onchange={(e) => dispatch('change', e)}
	onblur={(e) => dispatch('blur', e)}
	onfocus={(e) => dispatch('focus', e)}
/>
