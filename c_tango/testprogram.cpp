#include <c_tango.h>

int main() {
  std::cout << "sizeof(TangoDataType)=" << offsetof(AttributeData, attr_data) << "\n";
  
  void *proxy;
  
  ErrorStack *result = tango_create_device_proxy("tango://localhost:10000/sys/tg_test/1", &proxy);

  if (result) {
    std::cout << "error result length: " << result->length << "\n";
    return 1;
  }

  std::cout << "device proxy created\n";

  AttributeData argout;
  ErrorStack *read_result = tango_read_attribute(proxy, "string_scalar", &argout);
  
  if (read_result) {
    std::cout << "read error result length: " << result->length << "\n";
    return 2;
  }

  std::cout << "read sucessful, data type: " << argout.data_type << "\n";

  if (argout.data_type == 8) {
    std::cout << "read success result length: " << argout.attr_data.string_arr.length << "\n";
    std::cout << "string array, first element: " << argout.attr_data.string_arr.sequence[0] << "\n";
    std::cout << "dim x: " << argout.dim_x << "\n";
  }

  // argout.attr_data.double_arr.sequence[0] = 1337.0;
  // argout.attr_data.double_arr.length = 1;
  // tango_write_attribute(proxy, &argout);
  return 0;
}
