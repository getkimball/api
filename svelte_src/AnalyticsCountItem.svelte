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
             Row } from "sveltestrap";


    export let item = {};

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
          {sec.event} - {sec.count}
          <br />

          {/each}

        </CardBody>
        </Collapse>
</Card>
