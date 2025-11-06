<script lang="ts">
	import { user, login, logout } from '$lib/auth';
	import { _ } from 'svelte-i18n';
</script>

<header>
	<div class="title">{$_('calendar_title')}</div>

	{#if $user}
		<div class="auth">
			<span>{$user.name}</span>
			<button on:click={logout}>{$_('logout')}</button>
		</div>
	{:else}
		<div class="auth">
			<button on:click={login}>{$_('login')}</button>
		</div>
	{/if}
</header>

<style>
	header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 1rem;
		position: relative;
		overflow: visible;
		background-color: #f8f9fa;
		border-bottom: 1px solid #e9ecef;
	}

	.auth {
		display: flex;
		align-items: center;
		gap: 1rem;
	}

	.auth button {
		padding: 0.5rem 1rem;
		background-color: #0056a3;
		color: white;
		border: 1px solid #004080;
		border-radius: 0.25rem;
		cursor: pointer;
		font-weight: 500;
	}

	.auth button:hover {
		background-color: #004080;
	}

	.title {
		font-size: 1.25rem;
		font-weight: 600;
		color: var(--color-text);
	}
</style>
