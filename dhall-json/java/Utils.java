import java.io.IOException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;

public class Utils {

    public static String jsonToYaml (String jsonString) throws
        JsonProcessingException, IOException {
        JsonNode jsonNodeTree = new ObjectMapper().readTree(jsonString);
        String jsonAsYaml = new YAMLMapper().writeValueAsString(jsonNodeTree);
        return jsonAsYaml;
    }

     public static String yamlToJson (String yamlString) throws
        JsonProcessingException, IOException {
        JsonNode jsonNodeTree = new YAMLMapper().readTree(yamlString);
        String yamlAsJson = new ObjectMapper().writeValueAsString(jsonNodeTree);
        return yamlAsJson;
    }
}
