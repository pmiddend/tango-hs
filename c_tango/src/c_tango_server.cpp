#include "c_tango.h"
#include <unordered_map>

namespace
{
std::string this_device_name{"HaskellDevice"};

// Copy of the Tango definition in order to use std::string for more
// memory-safety (not strictly necessary, this thing)
struct AttributeDefinitionCpp
{
  std::string name;
  TangoDataType data_type;
  AttrWriteType write_type;
  void (*set_callback)(device_instance_ptr, void *);
  void (*get_callback)(device_instance_ptr, void *);
};

// This is global due to our "one device per executable" convention, and stores the initial state (and can be set from a normal global C function now)
Tango::DevState initial_state;
std::string initial_status;
void (*global_finalizer_callback)(void *);
void (*device_init_callback)(device_instance_ptr);

std::vector<AttributeDefinitionCpp> attribute_definitions;

std::vector<std::string> properties;

struct CommandDefinitionCpp
{
  std::string command_name;
  TangoDataType in_type;
  TangoDataType out_type;
  void *(*execute_callback)(device_instance_ptr, void *);
};

std::vector<CommandDefinitionCpp> command_definitions;

class HaskellCommandClass : public Tango::Command
{
public:
  HaskellCommandClass(
      std::string const &cmd_name,
      Tango::CmdArgType in,
      Tango::CmdArgType out,
      const char *in_desc,
      const char *out_desc,
      Tango::DispLevel level,
      void *(*execute_callback)(device_instance_ptr, void *))
      : Command(cmd_name, in, out, in_desc, out_desc, level), execute_callback{execute_callback}
  {
  }

  CORBA::Any *execute(Tango::DeviceImpl *dev, const CORBA::Any &in_any)
  {
    if (this->get_in_type() == Tango::DEV_VOID && this->get_out_type() == Tango::DEV_VOID)
    {
      this->execute_callback(dev, 0);
      return new CORBA::Any();
    }
    else if (this->get_in_type() == Tango::DEV_LONG64 && this->get_out_type() == Tango::DEV_VOID)
    {
      Tango::DevLong64 argin;
      extract(in_any, argin);

      this->execute_callback(dev, &argin);

      // Return value for void
      return new CORBA::Any();
    }
    else if (this->get_in_type() == Tango::DEV_DOUBLE && this->get_out_type() == Tango::DEV_VOID)
    {
      Tango::DevDouble argin;
      extract(in_any, argin);

      this->execute_callback(dev, &argin);

      // Return value for void
      return new CORBA::Any();
    }
    else if (this->get_in_type() == Tango::DEV_STRING && this->get_out_type() == Tango::DEV_VOID)
    {
      Tango::DevString argin;
      extract(in_any, argin);

      this->execute_callback(dev, argin);

      // Return value for void
      return new CORBA::Any();
    }
    else if (this->get_in_type() == Tango::DEV_VOID && this->get_out_type() == Tango::DEV_LONG64)
    {
      void *result = this->execute_callback(dev, 0);

      Tango::DevLong64 const number = *static_cast<Tango::DevLong64 *>(result);
      ::global_finalizer_callback(result);

      return this->insert(number);
    }
    else if (this->get_in_type() == Tango::DEV_VOID && this->get_out_type() == Tango::DEV_DOUBLE)
    {
      void *result = this->execute_callback(dev, 0);

      Tango::DevDouble const number = *static_cast<Tango::DevDouble *>(result);
      ::global_finalizer_callback(result);

      return this->insert(number);
    }
    else if (this->get_in_type() == Tango::DEV_VOID && this->get_out_type() == Tango::DEV_STRING)
    {
      char *const result = static_cast<char *>(this->execute_callback(dev, 0));
      size_t const len = strlen(result);

      char *copy = new char[len + 1];
      strncpy(copy, result, len);
      ::global_finalizer_callback(result);

      // This will "delete[]" the argument, hence our copy here.
      return this->insert(copy);
    }
    throw std::runtime_error("invalid type for command");
  }

  bool is_allowed(Tango::DeviceImpl *dev, const CORBA::Any &any) { return true; }

private:
  void *(*execute_callback)(device_instance_ptr, void *);
};

class JustOneAttributeClass : public Tango::DeviceClass
{
public:
  Tango::DbData cl_prop;
  Tango::DbData dev_def_prop;

  static JustOneAttributeClass *init(const char *);
  static JustOneAttributeClass *instance();

  Tango::DbDatum get_class_property(std::string &);
  Tango::DbDatum get_default_device_property(std::string &);

protected:
  JustOneAttributeClass(std::string &);
  void command_factory();
  void attribute_factory(std::vector<Tango::Attr *> &);
  static JustOneAttributeClass *_instance;

private:
  void device_factory(TANGO_UNUSED(const Tango::DevVarStringArray *));
  void create_static_attribute_list(std::vector<Tango::Attr *> &);
  std::vector<std::string> defaultAttList;
};

class JustOneAttribute : public TANGO_BASE_CLASS
{
public:
  // static JustOneAttribute *instance();

  JustOneAttribute(Tango::DeviceClass *cl, const char *s);
  ~JustOneAttribute();

  void delete_device();
  virtual void init_device();
  virtual void always_executed_hook();

  void store_user_data(void *user_data) { this->user_data = user_data; }

  void *get_user_data() { return this->user_data; }

  virtual void read_attr_hardware(std::vector<long> &attr_list);
  virtual void write_attr_hardware(std::vector<long> &attr_list);

  void add_dynamic_attributes();
  void add_dynamic_commands();

  std::string const &get_property_value(std::string const &s) const
  {
    return this->properties_values.at(s);
  }

private:
  JustOneAttributeClass &parent_class;
  std::unordered_map<std::string, std::string> properties_values;
  void *user_data;
  // static JustOneAttribute *_instance;
};

class haskellAttrib : public Tango::Attr
{
public:
  haskellAttrib(AttributeDefinitionCpp const &def)
      : Attr(
            def.name.c_str(),
            static_cast<long>(def.data_type),
            Tango::OPERATOR,
            static_cast<Tango::AttrWriteType>(def.write_type)),
        def{def},
        last_ptr{0},
        last_container{0}
  {
  }

  ~haskellAttrib() {}

  virtual void read(Tango::DeviceImpl *dev, Tango::Attribute &att)
  {
    if (def.data_type == DEV_LONG64)
    {
      TangoDevLong64 int_value;
      def.get_callback(dev, &int_value);
      att.set_value(&int_value);
    }
    else if (def.data_type == DEV_DOUBLE)
    {
      Tango::DevDouble value;
      def.get_callback(dev, &value);
      att.set_value(&value);
    }
    else if (def.data_type == DEV_BOOLEAN)
    {
      bool value;
      def.get_callback(dev, &value);
      att.set_value(&value);
    }
    else if (def.data_type == DEV_STRING)
    {
      std::cout << "haskell get callback (string)\n";
      // Future me: when you want to wrap this nicely, here's how this came to
      // be: We allocate a C string on the Haskell side, and get the pointer
      // here
      char *haskell_string;
      def.get_callback(dev, &haskell_string);

      // We also need a container for this C string, which we allocate on the
      // heap here.
      char **tango_string_array = new char *;

      // And fill it with one element
      *tango_string_array = haskell_string;

      // Then we tell Tango about this. It will remember both the container and
      // the element in it
      att.set_value(tango_string_array, 1, 0, false);

      // If this is the second "get", the we have a container and a string
      // leftover
      if (this->last_ptr != 0)
      {
        // The element was allocated with Haskell's malloc (or similar) and has
        // to be deleted from Haskell as well.
        global_finalizer_callback(this->last_ptr);
        this->last_ptr = 0;
        // The other element we allocated with new, so we can delete it here
        delete static_cast<char **>(this->last_container);
        this->last_container = 0;
      }
      this->last_ptr = haskell_string;
      this->last_container = tango_string_array;
    }
  }

  virtual void write(Tango::DeviceImpl *dev, Tango::WAttribute &att)
  {
    if (def.data_type == DEV_LONG64)
    {
      Tango::DevLong64 w_val;
      att.get_write_value(w_val);
      def.set_callback(dev, &w_val);
    }
    else if (def.data_type == DEV_DOUBLE)
    {
      Tango::DevDouble w_val;
      att.get_write_value(w_val);
      def.set_callback(dev, &w_val);
    }
    else if (def.data_type == DEV_BOOLEAN)
    {
      Tango::DevBoolean w_val;
      att.get_write_value(w_val);
      def.set_callback(dev, &w_val);
    }
    else if (def.data_type == DEV_STRING)
    {
      Tango::DevString w_val;
      att.get_write_value(w_val);
      def.set_callback(dev, &w_val);
    }
  }

  virtual bool is_allowed(Tango::DeviceImpl *dev, Tango::AttReqType ty) { return true; }

private:
  AttributeDefinitionCpp const &def;
  void *last_ptr;
  void *last_container;
};

JustOneAttributeClass *JustOneAttributeClass::_instance = NULL;

JustOneAttributeClass *JustOneAttributeClass::instance()
{
  if (_instance == NULL)
  {
    std::cerr << "Class is not initialised !!" << std::endl;
    exit(-1);
  }
  return _instance;
}

JustOneAttributeClass *JustOneAttributeClass::init(const char *name)
{
  if (_instance == NULL)
  {
    try
    {
      std::string s(name);
      _instance = new JustOneAttributeClass(s);
    }
    catch (std::bad_alloc &)
    {
      throw;
    }
  }
  return _instance;
}

Tango::DbDatum JustOneAttributeClass::get_class_property(std::string &prop_name)
{
  // This code seems a bit redundant: we don't really touch cl_prop,
  // so it must be empty. But okay, might make sense and I just don't
  // get it.
  for (unsigned int i = 0; i < cl_prop.size(); i++)
    if (cl_prop[i].name == prop_name)
      return cl_prop[i];
  //	if not found, returns  an empty DbDatum
  return Tango::DbDatum(prop_name);
}

Tango::DbDatum JustOneAttributeClass::get_default_device_property(std::string &prop_name)
{
  // This code seems a bit redundant: we don't really touch cl_prop,
  // so it must be empty. But okay, might make sense and I just don't
  // get it.
  for (unsigned int i = 0; i < dev_def_prop.size(); i++)
    if (dev_def_prop[i].name == prop_name)
      return dev_def_prop[i];
  //	if not found, returns  an empty DbDatum
  return Tango::DbDatum(prop_name);
}

JustOneAttributeClass::JustOneAttributeClass(std::string &s) : Tango::DeviceClass(s)
{
  std::cout << "JustOneAttributeClass(" << s << ")\n";
  TANGO_LOG_INFO << "Entering JustOneAttributeClass constructor" << std::endl;
  TANGO_LOG_INFO << "Leaving JustOneAttributeClass constructor" << std::endl;
}

void JustOneAttributeClass::command_factory()
{
  std::cout << "creating commands\n";
  for (CommandDefinitionCpp const &command_definition : command_definitions)
  {
    std::cout << "adding command " << command_definition.command_name << " (in type "
              << command_definition.in_type << ")\n";
    HaskellCommandClass *tango_command = new HaskellCommandClass(
        command_definition.command_name,
        static_cast<Tango::CmdArgType>(command_definition.in_type),
        static_cast<Tango::CmdArgType>(command_definition.out_type),
        "",
        "",
        Tango::OPERATOR,
        command_definition.execute_callback);
    command_list.push_back(tango_command);
  }
  std::cout << "finish adding commands\n";
}

void JustOneAttributeClass::attribute_factory(std::vector<Tango::Attr *> &att_list)
{
  for (AttributeDefinitionCpp const &attribute_definition : attribute_definitions)
  {
    std::cout << "creating attribute " << attribute_definition.name << "\n";
    haskellAttrib *new_attribute = new haskellAttrib(attribute_definition);
    Tango::UserDefaultAttrProp attribute_prop;
    //	description	not set for philipp
    // philipp_prop.set_label("mylabel");
    // philipp_prop.set_unit("myunit");
    // philipp_prop.set_standard_unit("mystandardunit");
    // philipp_prop.set_display_unit("mydisplayunit");
    //	format	not set for philipp
    //	max_value	not set for philipp
    //	min_value	not set for philipp
    //	max_alarm	not set for philipp
    //	min_alarm	not set for philipp
    //	max_warning	not set for philipp
    //	min_warning	not set for philipp
    //	delta_t	not set for philipp
    //	delta_val	not set for philipp
    new_attribute->set_default_properties(attribute_prop);
    //	Not Polled
    new_attribute->set_disp_level(Tango::OPERATOR);
    //	Not Memorized
    att_list.push_back(new_attribute);
    std::cout << "done creating attribute " << attribute_definition.name << "\n";
  }

  //	Create a list of static attributes
  std::cout << "creating static attribute list\n";
  create_static_attribute_list(get_class_attr()->get_attr_list());
  std::cout << "creating static attribute list done\n";
}

void JustOneAttributeClass::device_factory(const Tango::DevVarStringArray *devlist_ptr)
{
  std::cout << "creating devices\n";
  //	Create devices and add it into the device list
  for (unsigned long i = 0; i < devlist_ptr->length(); i++)
  {
    TANGO_LOG_DEBUG << "Device name : " << (*devlist_ptr)[i].in() << std::endl;
    device_list.push_back(new JustOneAttribute(this, (*devlist_ptr)[i]));
  }
  std::cout << "creating devices done\n";

  // Re-add if we have dynamic attributes
  // erase_dynamic_attributes(devlist_ptr, get_class_attr()->get_attr_list());

  //	Export devices to the outside world
  for (unsigned long i = 1; i <= devlist_ptr->length(); i++)
  {
    //	Add dynamic attributes if any
    JustOneAttribute *dev = static_cast<JustOneAttribute *>(device_list[device_list.size() - i]);
    dev->add_dynamic_attributes();

    //	Check before if database used.
    if ((Tango::Util::_UseDb == true) && (Tango::Util::_FileDb == false))
      export_device(dev);
    else
      export_device(dev, dev->get_name().c_str());
  }
}

void JustOneAttributeClass::create_static_attribute_list(std::vector<Tango::Attr *> &att_list)
{
  for (unsigned long i = 0; i < att_list.size(); i++)
  {
    std::string att_name(att_list[i]->get_name());
    transform(att_name.begin(), att_name.end(), att_name.begin(), ::tolower);
    defaultAttList.push_back(att_name);
  }

  TANGO_LOG_INFO << defaultAttList.size() << " attributes in default list" << std::endl;
}

JustOneAttribute::JustOneAttribute(Tango::DeviceClass *cl, const char *s)
    : TANGO_BASE_CLASS(cl, s, "description", initial_state, initial_status),
      parent_class(static_cast<JustOneAttributeClass &>(*cl)),
      properties_values{},
      user_data{0}
{
  init_device();

  Tango::DbData dev_prop;
  for (std::string const &property : ::properties)
  {
    dev_prop.push_back(Tango::DbDatum(property));
  }

  if (dev_prop.size() > 0)
  {
    //	Call database and extract values
    if (Tango::Util::instance()->_UseDb == true)
      get_db_device()->get_property(dev_prop);

    for (auto prop : dev_prop)
    {
      Tango::DbDatum cl_prop = parent_class.get_class_property(prop.name);
      std::vector<std::string> property_value;
      if (!cl_prop.is_empty())
        cl_prop >> property_value;
      else
      {
        Tango::DbDatum def_prop = parent_class.get_default_device_property(prop.name);
        if (!def_prop.is_empty())
          def_prop >> property_value;
      }
      // and try to extract kacke_property value from database
      if (!prop.is_empty())
        prop >> property_value;
      std::string final_string = "";
      for (auto s : property_value)
        final_string += s + "\n";
      this->properties_values[prop.name] = final_string;
    }
  }

  ::device_init_callback(this);
}

JustOneAttribute::~JustOneAttribute() { delete_device(); }

void JustOneAttribute::delete_device()
{
  DEBUG_STREAM << "JustOneAttribute::delete_device() " << device_name << std::endl;
  // delete[] attr_philipp_read;
}

void JustOneAttribute::init_device()
{
  DEBUG_STREAM << "JustOneAttribute::init_device() create device " << device_name << std::endl;
  // attr_philipp_read = new Tango::DevLong64[1];
}

void JustOneAttribute::always_executed_hook() {}

void JustOneAttribute::read_attr_hardware(TANGO_UNUSED(std::vector<long> &attr_list))
{
  DEBUG_STREAM << "JustOneAttribute::read_attr_hardware(std::vector<long> "
                  "&attr_list) entering... "
               << std::endl;
}

void JustOneAttribute::write_attr_hardware(TANGO_UNUSED(std::vector<long> &attr_list))
{
  DEBUG_STREAM << "JustOneAttribute::write_attr_hardware(std::vector<long> "
                  "&attr_list) entering... "
               << std::endl;
}

void JustOneAttribute::add_dynamic_attributes() {}

void JustOneAttribute::add_dynamic_commands() {}

// bool JustOneAttribute::is_philipp_allowed(TANGO_UNUSED(Tango::AttReqType
// type))
// {
//   return true;
// }
} // namespace

void Tango::DServer::class_factory()
{
  std::cout << "class_factory: adding " << ::this_device_name << "\n";
  add_class(JustOneAttributeClass::init(::this_device_name.c_str()));
}

void tango_server_add_attribute_definition(AttributeDefinition *definition)
{
  attribute_definitions.push_back(AttributeDefinitionCpp{
      definition->attribute_name,
      definition->data_type,
      definition->write_type,
      definition->set_callback,
      definition->get_callback,
  });
}

void tango_server_add_command_definition(CommandDefinition *definition)
{
  std::cout << "adding command to command definitions\n";
  command_definitions.push_back(CommandDefinitionCpp{
      definition->command_name,
      definition->in_type,
      definition->out_type,
      definition->execute_callback,
  });
}

int tango_server_init(
    int argc,
    char *argv[],
    void (*global_finalizer_callback)(void *),
    char *initial_status,
    int _initial_state,
    void (*device_init_callback)(device_instance_ptr))
{
  std::cout << "in tango_server_init" << std::endl;
  // Technically bad, since we're keeping memory for this initial
  // string indefinitely (we could clear it at some point), but it's
  // much more handy than keeping the externally delivered char
  // pointer
  ::initial_status = std::string{initial_status};
  ::initial_state = static_cast<Tango::DevState>(_initial_state);
  ::global_finalizer_callback = global_finalizer_callback;
  ::this_device_name = argv[0];
  ::device_init_callback = device_init_callback;

  Tango::Util *tg;
  try
  {
    // Initialise the device server
    //----------------------------------------
    std::cout << "Tango::Util::init(" << argc << "," << argv[0] << ")\n";
    tg = Tango::Util::init(argc, argv);

    // Create the device server singleton
    //	which will create everything
    //----------------------------------------
    tg->server_init(false);

    std::cout << "Server initialized" << std::endl;
  }
  catch (std::bad_alloc &)
  {
    std::cout << "Can't allocate memory to store device object !!!" << std::endl;
    std::cout << "Exiting" << std::endl;
  }
  catch (CORBA::Exception &e)
  {
    Tango::Except::print_exception(e);

    std::cout << "Received a CORBA_Exception" << std::endl;
    std::cout << "Exiting" << std::endl;
  }

  return (0);
}

void tango_server_start()
{
  Tango::Util *tg = Tango::Util::instance();
  if (!tg)
    return;
  try
  {
    // Run the endless loop
    //----------------------------------------
    std::cout << "Ready to accept request" << std::endl;
    tg->server_run();
  }
  catch (std::bad_alloc &)
  {
    std::cout << "Can't allocate memory to store device object !!!" << std::endl;
    std::cout << "Exiting" << std::endl;
  }
  catch (CORBA::Exception &e)
  {
    Tango::Except::print_exception(e);

    std::cout << "Received a CORBA_Exception" << std::endl;
    std::cout << "Exiting" << std::endl;
  }

  if (tg)
  {
    tg->server_cleanup();
  }
}

void tango_server_set_status(device_instance_ptr instance, char *new_status)
{
  // Memeory-wise, this is fine, since set_status gets a std::string.
  static_cast<JustOneAttribute *>(instance)->set_status(new_status);
}

void tango_server_set_state(device_instance_ptr instance, int const new_state)
{
  static_cast<JustOneAttribute *>(instance)->set_state(static_cast<Tango::DevState>(new_state));
}

void tango_server_add_property(char *property_name)
{
  ::properties.push_back(std::string{property_name});
}

char const *tango_server_read_property(device_instance_ptr instance, char *property_name)
{
  return static_cast<JustOneAttribute *>(instance)->get_property_value(property_name).c_str();
}

void tango_server_store_user_data(device_instance_ptr ptr, void *data)
{
  static_cast<JustOneAttribute *>(ptr)->store_user_data(data);
}

void *tango_server_get_user_data(device_instance_ptr ptr)
{
  return static_cast<JustOneAttribute *>(ptr)->get_user_data();
}
