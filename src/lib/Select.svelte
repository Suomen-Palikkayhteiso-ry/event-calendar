<script lang="ts">
	import { createEventDispatcher } from 'svelte';

	interface Props {
		value?: string;
		required?: boolean;
		disabled?: boolean;
		ariaLabel?: string;
		ariaDescribedBy?: string;
		ariaInvalid?: boolean;
		id?: string;
		name?: string;
		children?: any;
	}

	let {
		value = '',
		required = false,
		disabled = false,
		ariaLabel,
		ariaDescribedBy,
		ariaInvalid = false,
		id,
		name,
		children
	}: Props = $props();

	const dispatch = createEventDispatcher();
</script>

<select
	bind:value
	{required}
	{disabled}
	aria-label={ariaLabel}
	aria-describedby={ariaDescribedBy}
	aria-invalid={ariaInvalid}
	{id}
	{name}
	class="form-input"
	onchange={(e) => dispatch('change', e)}
	onblur={(e) => dispatch('blur', e)}
	onfocus={(e) => dispatch('focus', e)}
>
	{@render children?.()}
</select>