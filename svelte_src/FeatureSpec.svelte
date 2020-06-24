<script>
    import { Col,
             Collapse,
             Card,
             CardBody,
             CardFooter,
             CardHeader,
             CardSubtitle,
             CardText,
             CardTitle } from "sveltestrap";

    export let spec = {};
    let isOpen = false;
    function toggle() {
        isOpen = !isOpen;
    };
</script>

<main>
    <Col xs="4" align="center">
    <Card>
        <CardHeader on:click={toggle} >
            <CardTitle>{spec.name}</CardTitle>
        </CardHeader>
        <Collapse {isOpen}>
        <CardBody>
            Boolean Default: {spec.boolean}
        </CardBody>
        </Collapse>

    </Card>
    </Col>
</main>

<style>
</style>
