<script lang="ts">
	import { importKML } from '$lib/kml-utils';

	interface Props {
		kmlFile: File | null;
		isImporting: boolean;
		onFileChange: (file: File | null) => void;
		onImport: () => void;
	}

	let { kmlFile, isImporting, onFileChange, onImport }: Props = $props();

	async function handleImport() {
		if (!kmlFile) return;
		await importKML(kmlFile, onImport);
	}
</script>

<div class="mb-8">
	<h2 class="mb-4 text-gray-900">Import KML</h2>
	<div class="mb-4">
		<label for="kmlFile" class="mb-2 block font-medium text-gray-700">KML File</label>
		<input
			type="file"
			id="kmlFile"
			accept=".kml"
			disabled={isImporting}
			onchange={(e) => onFileChange((e.target as HTMLInputElement).files?.[0] || null)}
			class="focus:ring-opacity-25 box-border w-full rounded border border-gray-300 p-3 text-base focus:border-brand-primary focus:ring-2 focus:ring-brand-primary focus:outline-none"
		/>
	</div>
	<button
		onclick={handleImport}
		disabled={!kmlFile || isImporting}
		class="cursor-pointer rounded border border-primary-500 bg-primary-500 px-6 py-3 text-base text-white transition-colors hover:bg-primary-600 disabled:cursor-not-allowed disabled:border-gray-400 disabled:bg-gray-200 disabled:text-gray-600 disabled:hover:bg-gray-300"
	>
		{isImporting ? 'Importing...' : 'Import KML'}
	</button>
</div>
