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
        const prediction_res = await fetch(`v0/predictions`);
        const prediction_json = await prediction_res.json();
        analytics = json.counts.sort((a, b) => a.count < b.count)

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

            console.log(prediction_json["goals"][goal.name]["events"]);
            predictions[goal.name][event.name] = prediction_json["goals"][goal.name]["events"][event.name]["bayes"];
          }

        }

    });

</script>

<Row>

<Col>
<Table>
    <thead>
        <th>Event</th>
        <th>Count</th>
    </thead>
    <tbody>
        {#each analytics as analyticItem }
        <tr>
        <td scope="row">{analyticItem.name}</td>
        <td>{analyticItem.count}</td>

        {/each}
    </tbody>
</Table>
</Col>
</Row>
