<script lang="ts">
	import { user, login, logout } from '$lib/auth';
</script>

<header>
	<div class="corner">
		<a href="/">
			<img src="/logo.svg" alt="Suomen Palikkayhteisö ry" class="logo" />
		</a>
	</div>

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
		height: 6em;
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
		background-color: var(--color-theme);
		color: white;
		border: none;
		border-radius: 0.25rem;
		cursor: pointer;
		font-weight: 500;
	}

	.auth button:hover {
		background-color: #004080;
	}
</style>
