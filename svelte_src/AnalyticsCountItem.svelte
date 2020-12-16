<script>
    import { onMount } from "svelte";
    import { Button,
             Card,
             CardBody,
             CardHeader,
             CardTitle,
             Col,
             Collapse,
             Container,
             CustomInput,
             FormGroup,
             Input,
             Progress,
             Row } from "sveltestrap";


    export let item = {};
    export let probabilities;
    let events = item.single_event_counts;

    let included_event_names = [];
    let excluded_event_names = [];

    let included_counts = 0;
    let excluded_counts = 0;
    let ignored_counts = 0;

    let included_goal_event_counts = [];
    let excluded_goal_event_counts = [];
    let ignored_goal_event_counts = [];

    let isOpen = false;

    function filter_updated(el, e) {
        let val = el.target.value;
        if (val === "1") filter_add_event(e)
        else if (val ==="-1") filter_remove_event(e)
        else if (val ==="0") filter_reset_event(e)
        else  {console.log("Val is unhandled", val)}
    }

    function filter_add_event(e) {
        e.added = true;
        e.removed = false;
        events = events; // Make sure events looks like it was updated
        update_added_removed();
        update_filtered_event_counts();
    }
    function filter_remove_event(e) {
        e.added = false;
        e.removed = true;
        events = events; // Make sure events looks like it was updated
        update_added_removed();
        update_filtered_event_counts();
    }
    function filter_reset_event(e) {
        e.added = false;
        e.removed = false;
        events = events; // Make sure events looks like it was updated
        update_added_removed();
        update_filtered_event_counts();
    }
    function update_added_removed() {
        let new_include = {};
        let new_exclude = {};
        for (event of events) {
            if (event.added) {
                new_include[event.name] = undefined;
            }
            else if (event.removed) {
                new_exclude[event.name] = undefined;
            }
        }
        included_event_names = new_include;
        excluded_event_names = new_exclude;
    }

    function update_filtered_event_counts() {
        let new_included_goal_event_counts = [];
        let new_excluded_goal_event_counts = [];
        let new_ignored_goal_event_counts = [];

        let event_count;
        for (event_count of item.event_counts) {
            let event;
            let excluded = false;
            let included = false;
            for (event of event_count.events) {

                // Check to see if the event should be excluded
                if (excluded_event_names.hasOwnProperty(event)) {
                    excluded = true;
                    break;
                }
                // See if the event count includes this event
                else if (included_event_names.hasOwnProperty(event)) {
                    included = true;
                }

            }
            if (excluded) {
                new_excluded_goal_event_counts.push(event_count);
            }
            else if (included) {
                new_included_goal_event_counts.push(event_count);
            }
            else {
                new_ignored_goal_event_counts.push(event_count);
            }
        }
        included_goal_event_counts = new_included_goal_event_counts;
        included_counts = sum_event_counts(new_included_goal_event_counts);

        excluded_goal_event_counts = new_excluded_goal_event_counts;
        excluded_counts = sum_event_counts(new_excluded_goal_event_counts);

        ignored_goal_event_counts = new_ignored_goal_event_counts;
        ignored_counts = sum_event_counts(new_ignored_goal_event_counts);
    }

    function sum_event_counts(counts) {
        let sum = 0;
        let count;
        for (count of counts) {
            sum += count.count;
        }
        return sum;
    }

</script>

<Card>

        <CardHeader on:click={() => (isOpen = !isOpen)} >
            <CardTitle>{item.name}</CardTitle>
        </CardHeader>
        <Collapse {isOpen}>
        <CardBody>
          Unique users: {item.count}
          <br />
          <br />
          {#if item.single_event_counts.length > 0 }
            <br />
            {#each events as event (event) }
                <Card><CardBody>
                <Row><Col>{event.name}: </Col></Row>
                <Row>
                    <Col></Col><Col>NOT</Col>

                    <Col xs=4>
                    <CustomInput
                        type="range"
                        id="{item.name}-{event.name}-slider"
                        min="-1"
                        max="1"
                        label="Include/exclude this event"
                        bind:value={event.ui_val}
                        on:change={e => (filter_updated(e, event))} />
                    </Col>
                    <Col >OR</Col><Col></Col>
                </Row>
                </CardBody></Card>
            {/each}
            <br />
            <Row>
                <Col>{excluded_counts}</Col>
                <Col>{ignored_counts}</Col>
                <Col>{included_counts}</Col>
            </Row>

          {/if}

        </CardBody>
        </Collapse>
</Card>
