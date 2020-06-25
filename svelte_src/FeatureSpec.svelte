<script>
    import { Button,
             Col,
             Collapse,
             Card,
             CardBody,
             CardFooter,
             CardHeader,
             CardSubtitle,
             CardText,
             CardTitle,
             Input,
             ListGroup,
             ListGroupItem,
             Form,
             FormGroup,
             Row} from "sveltestrap";

    export let spec = {};
    let isOpen = false;
    function toggle() {
        isOpen = !isOpen;
    };
    async function save() {
        let response = await fetch('/v0/featureSpecs/', {
            method: 'POST',
            headers: {
                'content-type': 'application/json'
            },
            body: JSON.stringify(spec)
        });
        console.log(await response.json());
        alert(await response.status);

    }
</script>

<main>
    <Col xs="4" align="center">
    <Card>
        <CardHeader on:click={toggle} >
            <CardTitle>{spec.name}</CardTitle>
        </CardHeader>
        <Collapse {isOpen}>
        <CardBody>
            <form on:submit|preventDefault={save}><FormGroup>
            <Row>
                <Col>Boolean Enabled</Col>
                <Col><Input type="checkbox" bind:checked={spec.boolean} /></Col>
                <Col>{spec.boolean}</Col>
            </Row>

            {#if spec.rollout_start } <Row>
            Rollout Start:<Input
                type="datetime"
                name="rollout_start"
                id="exampleDatetime"
                placeholder="{spec.rollout_start}"
                value="{spec.rollout_start}" />
            </Row> {/if}

            {#if spec.rollout_start }<Row>
            Rollout End: <Input
                type="datetime"
                name="rollout_end"
                id="exampleDatetime"
                placeholder="{spec.rollout_end}"
                value="{spec.rollout_end}" />
            </Row>{/if}

            {#if spec.user != []}<Row>
                <ListGroup>
                {#each spec.user as userSpec }
                <ListGroupItem>{userSpec[0]} {userSpec[1]} {userSpec[2]}</ListGroupItem>
                {/each}
                </ListGroup>
            </Row>{/if}

            <Row>
                <Button type="submit" >Save</Button>
            </Row>

        </FormGroup></form>
        </CardBody>
        </Collapse>

    </Card>
    </Col>
</main>

<style>
</style>
