<script>
    import { onMount } from "svelte";

    let featureSpecs = [];
    onMount(async () => {
        const res = await fetch(`v0/featureSpecs`);
        const json = await res.json();
        featureSpecs = json.featureSpecs;
    });

</script>

<main>
	<h1>Kimball Features</h1>
    <ul>
    {#each featureSpecs as featureSpec }
        <li>{featureSpec.name} - {featureSpec.boolean}</li>

    {/each}
    </ul>

</main>

<style>
	main {
		text-align: center;
		padding: 1em;
		max-width: 240px;
		margin: 0 auto;
	}

	h1 {
		color: #ff3e00;
		text-transform: uppercase;
		font-size: 4em;
		font-weight: 100;
	}

	@media (min-width: 640px) {
		main {
			max-width: none;
		}
	}
</style>
