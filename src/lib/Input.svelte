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
		ariaRequired?: boolean;
		id?: string;
		name?: string;
		pattern?: string;
		min?: number | string;
		max?: number | string;
		step?: number | string;
		accept?: string;
		autofocus?: boolean;
		class?: string;
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
		ariaRequired = false,
		id,
		name,
		pattern,
		min,
		max,
		step,
		accept,
		autofocus = false,
		class: className,
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
	aria-required={ariaRequired}
	{id}
	{name}
	{pattern}
	{min}
	{max}
	{step}
	{accept}
	{autofocus}
	class="form-input {className}"
	oninput={(e) => dispatch('input', e)}
	onchange={(e) => dispatch('change', e)}
	onblur={(e) => dispatch('blur', e)}
	onfocus={(e) => dispatch('focus', e)}
/>
