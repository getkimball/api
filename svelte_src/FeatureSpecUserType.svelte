<script>
    import {

             Col,
             Collapse,
             Dropdown,
             DropdownItem,
             DropdownMenu,
             DropdownToggle,
             Input,
             FormGroup,
             ListGroup,
             ListGroupItem,
             Row} from "sveltestrap";

    export let userSpec = [];

    let comparatorType = typeof(userSpec.comparator);

    let isOpen = false;

</script>


<main>

  <ListGroup>
  <ListGroupItem>
      <Input bind:value={userSpec.property} />

      <FormGroup>
        <Input type=select bind:value="{userSpec.comparator}">
          <option>=</option>
          <option>in</option>
        </Input>
      </FormGroup>

      <FormGroup>
        <Input type=select bind:value="{comparatorType}">
          <option>string</option>
        </Input>
      </FormGroup>



      <Input bind:value={userSpec.value} />
  </ListGroupItem>
  </ListGroup>

</main>

<style>
</style>
