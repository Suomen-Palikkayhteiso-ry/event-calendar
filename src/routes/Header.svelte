<script lang="ts">
	import { user, login, logout } from '$lib/auth';
	import { _ } from 'svelte-i18n';
	import { resolve } from '$app/paths';
</script>

<header
	class="relative flex items-center justify-between overflow-visible border-b border-gray-200 bg-gray-50 p-4"
>
	<a
		href={resolve('/')}
		class="text-xl font-semibold text-gray-700 no-underline hover:text-brand-primary"
		>{$_('calendar_title')}</a
	>

	{#if $user}
		<div class="flex items-center gap-4">
			<span class="text-gray-700">{$user.name}</span>
			<button
				class="cursor-pointer rounded border border-gray-300 bg-gray-600 px-4 py-2 font-medium text-white hover:bg-gray-700"
				on:click={logout}>{$_('logout')}</button
			>
		</div>
	{:else}
		<div class="flex items-center gap-4">
			<button
				class="cursor-pointer rounded border border-brand-accent bg-brand-secondary px-4 py-2 font-medium text-white hover:bg-brand-accent"
				on:click={login}>{$_('login')}</button
			>
		</div>
	{/if}
</header>
