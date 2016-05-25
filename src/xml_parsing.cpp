#include <Rcpp.h>
#include "pugixml.h"

using namespace pugi;

//
// http://pugixml.org/docs/manual.html
//

// http://stackoverflow.com/questions/7140793/how-to-convert-pugichar-t-to-string
std::string as_utf8(const char* str) { return str; }
std::wstring as_wide(const wchar_t* str) { return str; }

// PWX ---------------------------------------------------------------------- //
// http://www.trainingpeaks.com/PWX/1/0/pwx.xsd

// [[Rcpp::export]]
Rcpp::List PARSE_PWX(const char* file_path)
{
  /*
   *    Document structure is:
   *    /pwx/workout/sample/.
   */

  xml_document doc;
  Rcpp::List out;           // Returned list.
  double i = 0;             // For appending to out list.
  std::string metric_name;  // Node names are char_t*

  if (!doc.load_file(file_path)) return out; // Bad file (return empty list).

  xpath_node_set samples = doc.select_nodes("//sample");  // XPath Iterator.

  // Node declarations:
  xml_node this_node, metric, extensions, ext;

  out = Rcpp::List(samples.size());
  for (xpath_node_set::const_iterator it = samples.begin();
       it != samples.end();
       ++it, ++i)
  {
    Rcpp::checkUserInterrupt();  // Just in case.

    xpath_node sample = *it;
    this_node = sample.node();

    // Named vector to hold everything from this Trackpoint.
    Rcpp::CharacterVector tmp = Rcpp::CharacterVector::create();

    // Traverse the contents of this trackpoint.
    for (metric = this_node.first_child();
         metric;
         metric = metric.next_sibling())
    {
      metric_name = as_utf8(metric.name());
      tmp[metric_name] = metric.child_value();
    }
    out[i] = tmp;
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::CharacterVector PWX_START_TIME(const char* file_path)
{
  pugi::xml_document doc;
  if (!doc.load_file(file_path)) return "";  // Bad file (return zchar).
  pugi::xml_node start_time = doc.child("pwx").child("workout").child("time");

  return start_time.child_value();
}

// TCX ---------------------------------------------------------------------- //

// [[Rcpp::export]]
Rcpp::List PARSE_TCX(const char* file_path)
{
  /*
   *    Document structure is:
   *    /TrainingCenterDatabase/Activities/Activity/Lap/Track/Trackpoint/.
   */

  xml_document doc;
  Rcpp::List out;           // Returned list.
  double i = 0;             // For appending to out list.
  std::string metric_name,  // Node names are char_t*
  ext_name;

  if (!doc.load_file(file_path)) return out; // Bad file (return empty list).

  // Find trackpoints with an XPath expression
  // (lap triggers preclude using the node.next_sibling() method).
  xpath_node_set trkpoints = doc.select_nodes("//Trackpoint");

  // Node declarations:
  xml_node this_node, metric, extensions, ext;

  out = Rcpp::List(trkpoints.size());
  for (xpath_node_set::const_iterator it = trkpoints.begin();
       it != trkpoints.end();
       ++it, ++i)
  {
    Rcpp::checkUserInterrupt();  // Just in case.

    xpath_node trkpoint = *it;
    this_node = trkpoint.node();

    // Named vector to hold everything from this Trackpoint.
    Rcpp::CharacterVector tmp = Rcpp::CharacterVector::create();

    // Traverse the contents of this trackpoint.
    for (metric = this_node.first_child();
         metric;
         metric = metric.next_sibling())
    {
      metric_name = as_utf8(metric.name());  // For comparisons.

      if (metric_name == "HeartRateBpm")
      {
        tmp[metric_name] = metric.child("Value").child_value();
      }
      else if (metric_name == "Position")
      {
        tmp["LatitudeDegrees"]  = metric.child("LatitudeDegrees").child_value();
        tmp["LongitudeDegrees"] = metric.child("LongitudeDegrees").child_value();
      }
      else if (metric_name == "Extensions")
      {
        extensions = metric.last_child(); // To the bottom.
        for (ext = extensions.first_child(); ext; ext = ext.next_sibling())
        {
          ext_name = as_utf8(ext.name());
          tmp[ext_name] = ext.child_value();
        }
      }
      else  // "Top level" metrics.
      {
        tmp[metric_name] = metric.child_value();
      }
    }
    out[i] = tmp;
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::List TCX_LAPS(const char* file_path)
{
  xml_document doc;
  Rcpp::List out;           // Returned list.
  double i = 0;

  if (!doc.load_file(file_path)) return out; // Bad file (return empty list).

  xpath_node_set triggers = doc.select_nodes("//Lap[@StartTime]");

  out = Rcpp::List(triggers.size());

  for (xpath_node_set::const_iterator it = triggers.begin();
       it != triggers.end();
       ++it, ++i)
  {
    Rcpp::checkUserInterrupt();  // Just in case.
    xpath_node trigger = *it;
    out[i] = trigger.node().attribute("StartTime").value();
  }

  return out;
}

// GPX ---------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List PARSE_GPX(const char* file_path)
{
  /*
   *    Document structure is:
   *    /gpx/trk/trkseg/trkpt/.
   */

  xml_document doc;
  Rcpp::List out;           // Returned list.
  double i = 0;             // For appending to out list.
  std::string metric_name,  // Node names are char_t*
  ext_name;

  if (!doc.load_file(file_path)) return out; // Bad file (return empty list).

  // Find trackpoints with an XPath expression.
  xpath_node_set trkpoints = doc.select_nodes("//trkpt");

  // Node declarations:
  xml_node this_node, metric, extensions, ext;

  out = Rcpp::List(trkpoints.size());
  for (xpath_node_set::const_iterator it = trkpoints.begin();
       it != trkpoints.end();
       ++it, ++i)
  {
    Rcpp::checkUserInterrupt();  // Just in case.

    xpath_node trkpoint = *it;
    this_node = trkpoint.node();

    // Named vector to hold everything from this Trackpoint.
    Rcpp::CharacterVector tmp = Rcpp::CharacterVector::create();

    tmp["lon"] = this_node.attribute("lon").value();
    tmp["lat"] = this_node.attribute("lat").value();

    // Traverse the contents of this trackpoint.
    for (metric = trkpoint.node().first_child();
         metric;
         metric = metric.next_sibling())
    {
      metric_name = as_utf8(metric.name());  // For comparisons.

      if (metric_name == "extensions")
      {
        extensions = metric.last_child(); // To the bottom.
        for (ext = extensions.first_child(); ext; ext = ext.next_sibling())
        {
          ext_name = as_utf8(ext.name());
          tmp[ext_name] = ext.child_value();
        }
      }
      else  // "Top level" metrics.
      {
        tmp[metric_name] = metric.child_value();
      }
    }
    out[i] = tmp;
  }
  return out;
}
