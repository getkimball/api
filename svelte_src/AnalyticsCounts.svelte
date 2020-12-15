<script>
    import { onMount } from "svelte";
    import { Col,
             Input,
             Label,
             Row,
             Table} from "sveltestrap";

    import AnalyticsCountItem from './AnalyticsCountItem.svelte';
    import AnalyticsPrediction from './AnalyticsCountItem.svelte';
    let analytics = [];
    let analyticLookup = {};
    let goals = [];
    let namespace = "default";
    let namespaces = ["default"];
    let predictions = {};

    async function update_from_api() {
        const ns_res = await fetch('v0/namespaces');
        const ns_json = await ns_res.json();
        namespaces = ns_json['namespaces'].sort();

        const analytics_res = await fetch(`v0/analytics?namespace=` + namespace);
        const analytics_json = await analytics_res.json();

        const prediction_res = await fetch(`v0/predictions?namespace=`+ namespace);
        const prediction_json = await prediction_res.json();
        analytics = analytics_json.counts.sort((a, b) => a.count < b.count)


        goals = [];
        predictions = {};

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
            predictions[goal.name][event.name] = prediction_json["goals"][goal.name]["events"][event.name]["bayes"];
          }

        }

    };

    onMount(async () => {
        update_from_api();

    });

    function namespace_update(ns) {
        update_from_api();

    }
    $: namespace_update(namespace);

</script>
<Row><Col>
    <Label for="namespaceSelect">Namespace</Label>
    <Input type="select" name="select" id="namespaceSelect" bind:value={namespace}>
    {#each namespaces as namespaceItem }
      <option value={namespaceItem}>{namespaceItem}</option>
    {/each}
    </Input>

</Col></Row>


<Row>

<Col>
    <Row><Col><h2>Events</h2></Col></Row>

    <Row><Col>
    <Table>
    <thead>
        <th>Event</th>
        <th>Count</th>
    </thead>
    <tbody>
        {#each analytics as analyticItem }
        <tr>
          <td>{analyticItem.name}</td>
          <td>{analyticItem.count}</td>
        </tr>
        {/each}
    </tbody>
    </Table>
    </Col></Row>
</Col>


<Col>
    <Row><Col><h2>Goals</h2></Col></Row>

    <Row><Col>
   {#each goals as analyticItem (analyticItem) }
       <AnalyticsCountItem item={analyticItem} probabilities={predictions[analyticItem.name]} />
   {/each}
    </Col></Row>
</Col>

</Row>
