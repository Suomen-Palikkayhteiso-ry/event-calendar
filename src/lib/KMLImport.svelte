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
		<label for="kmlFile" class="form-label">KML File</label>
		<input
			type="file"
			id="kmlFile"
			accept=".kml"
			disabled={isImporting}
			onchange={(e) => onFileChange((e.target as HTMLInputElement).files?.[0] || null)}
			class="form-input"
		/>
	</div>
	<button onclick={handleImport} disabled={!kmlFile || isImporting} class="btn-primary">
		{isImporting ? 'Importing...' : 'Import KML'}
	</button>
</div>
