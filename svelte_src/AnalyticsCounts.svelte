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
        analytics = json.counts.sort((a, b) => a.name.toLowerCase() > b.name.toLowerCase())
    });

</script>

<Row>

<Col>
{#each analytics as analyticItem }
<Row>
  <Col></Col>
  <Col xs="6">
  <AnalyticsCountItem item={analyticItem} />
  </Col>
  <Col></Col>
</Row>
{:else}
    No feature analytics yet
    Make sure "user_id" and "feature" are set
{/each}


</Col>
</Row>
