<script>
    import { onMount } from "svelte";
    import { Col,
             Row,
             Table} from "sveltestrap";

    import AnalyticsCountItem from './AnalyticsCountItem.svelte';
    import AnalyticsPrediction from './AnalyticsCountItem.svelte';
    let analytics = [];
    let analyticLookup = {};
    let goals = [];
    let predictions = {};

    onMount(async () => {
        const res = await fetch(`v0/analytics`);
        const json = await res.json();
        analytics = json.counts.sort((a, b) => a.name.toLowerCase() > b.name.toLowerCase())

        goals = analytics.filter(obj => obj.single_event_counts.length > 0);

        analyticLookup = analytics.reduce(function(map, obj) {
            map[obj.name] = obj.count;
            return map;
        }, {});

        let global_count = analyticLookup["global_counter"];

        let goal;
        let event;
        let probability;
        for (goal of goals) {
          predictions[goal.name] = {};
          for (event of goal.single_event_counts) {

            probability = ((event.count / goal.count) * (goal.count / global_count)) / (analyticLookup[event.event] / global_count);
            predictions[goal.name][event.event] = probability;
          }

        }

    });

</script>

<Row>

<Col>
{#each analytics as analyticItem }
<Row>
  <Col></Col>
  <Col xs="10">
  <AnalyticsCountItem item={analyticItem} probabilities={predictions[analyticItem.name]}/>
  </Col>
  <Col></Col>
</Row>
{:else}
    No feature analytics yet
    Make sure "user_id" and "feature" are set
{/each}
</Col>
</Row>
