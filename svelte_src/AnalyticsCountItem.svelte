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
             FormGroup,
             Input,
             Progress,
             Row } from "sveltestrap";


    export let item = {};
    export let probabilities;

    let isOpen = false;

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
          Before doing this users also did:
          <br />
          {#each item.single_event_counts as sec }
          {sec.event}
          <br />
          {#if probabilities[sec.event]}
          {probabilities[sec.event] * 100}% probable that users that complete this event will complete this goal
          {/if}
          <Progress value={sec.count / item.count * 100}>{sec.count}</Progress>
          <br />

          {/each}

        </CardBody>
        </Collapse>
</Card>
