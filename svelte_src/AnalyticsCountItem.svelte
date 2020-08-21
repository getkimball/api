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

        </CardBody>
        </Collapse>
</Card>
