<script>
    import { Col,
             Collapse,
             Card,
             CardBody,
             CardFooter,
             CardHeader,
             CardSubtitle,
             CardText,
             CardTitle,
             Input,
             Row} from "sveltestrap";

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
            <Row>
            Boolean Default: {spec.boolean}
            </Row>

            {#if spec.rollout_start != 'undefined' } <Row>
            Rollout Start:<Input
                type="datetime"
                name="rollout_start"
                id="exampleDatetime"
                placeholder="{spec.rollout_start}"
                value="{spec.rollout_start}" />
            </Row> {/if}

            {#if spec.rollout_start != 'undefined' }<Row>
             Rollout End: <Input
                type="datetime"
                name="rollout_end"
                id="exampleDatetime"
                placeholder="{spec.rollout_end}"
                value="{spec.rollout_end}" />
            </Row>{/if}
        </CardBody>
        </Collapse>

    </Card>
    </Col>
</main>

<style>
</style>
