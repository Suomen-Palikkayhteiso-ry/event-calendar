<script lang="ts">
	interface Props {
		htmlFor?: string;
		required?: boolean;
		children?: any;
	}

	let { htmlFor, required = false, children }: Props = $props();
</script>

<label for={htmlFor} class="form-label">
	{@render children?.()}
	{#if required}
		<span class="text-error" aria-label="required">*</span>
	{/if}
</label>
