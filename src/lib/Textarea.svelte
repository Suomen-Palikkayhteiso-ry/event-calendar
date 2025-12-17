<script lang="ts">
	import { createEventDispatcher } from 'svelte';

	interface Props {
		value?: string;
		placeholder?: string;
		required?: boolean;
		disabled?: boolean;
		readonly?: boolean;
		rows?: number;
		cols?: number;
		ariaLabel?: string;
		ariaDescribedBy?: string;
		ariaInvalid?: boolean;
		id?: string;
		name?: string;
		children?: any;
	}

	let {
		value = '',
		placeholder,
		required = false,
		disabled = false,
		readonly = false,
		rows = 3,
		cols,
		ariaLabel,
		ariaDescribedBy,
		ariaInvalid = false,
		id,
		name,
		children
	}: Props = $props();

	const dispatch = createEventDispatcher();
</script>

<textarea
	bind:value
	{placeholder}
	{required}
	{disabled}
	{readonly}
	{rows}
	{cols}
	aria-label={ariaLabel}
	aria-describedby={ariaDescribedBy}
	aria-invalid={ariaInvalid}
	{id}
	{name}
	class="form-input"
	oninput={(e) => dispatch('input', e)}
	onchange={(e) => dispatch('change', e)}
	onblur={(e) => dispatch('blur', e)}
	onfocus={(e) => dispatch('focus', e)}
></textarea>
