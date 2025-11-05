<script lang="ts">
	import { user, login, logout } from '$lib/auth';
	import { resolve } from '$app/paths';
</script>

<header>
	<div class="corner">
		<a href={resolve('/')}>
			<img src="/logo.svg" alt="Suomen Palikkayhteisö ry" class="logo" />
		</a>
	</div>

	<div class="title">Palikkakalenteri</div>

	{#if $user}
		<div class="auth">
			<span>Hello, {$user.name}</span>
			<button on:click={logout}>Logout</button>
		</div>
	{:else}
		<div class="auth">
			<button on:click={login}>Login</button>
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

	.corner {
		width: 3em;
		height: 3em;
	}

	.corner a {
		display: flex;
		align-items: center;
		justify-content: center;
		width: 100%;
		height: 100%;
	}

	.logo {
		height: 4.5em;
		width: auto;
		position: absolute;
		top: 0.5em;
		left: 0.5em;
		z-index: 10;
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
