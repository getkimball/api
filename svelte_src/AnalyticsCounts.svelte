<script>
    import { onMount } from "svelte";
    import { Col,
             Row,
             Table} from "sveltestrap";

    import AnalyticsCountItem from './AnalyticsCountItem.svelte';
    let analytics = [];

    onMount(async () => {
        const res = await fetch(`v0/analytics`);
        const json = await res.json();
        analytics = json.counts;
    });

</script>

<Row>

<h2>Analytics</h2>
<Table>
  <thead>
    <th>Feature<th>
    <th>Count</th>
  </thead>

<tbody>
{#each analytics as analyticItem }
<tr>

    <td scope="row">{analyticItem.name}</td>
    <td>{analyticItem.count}</td>
</tr>
{:else}
    <td>No feature analytics yet</td>
    <td>Make sure "user_id" and "feature" are set</td>
{/each}

</tbody>

</Table>

</Row>
